// =============================================================================
// CourtSide Stats - Live Entry
// Runs in the browser: every tap is recorded on this device at once, shown at
// once, and sent to the server in the background. If the connection drops,
// plays wait in this device's storage and are sent when it comes back.
// =============================================================================
(function () {
  'use strict';
  var LS = window.localStorage;
  var store = {
    get: function (k, d) { try { var v = LS.getItem(k); return v === null ? d : JSON.parse(v); } catch (e) { return d; } },
    set: function (k, v) { try { LS.setItem(k, JSON.stringify(v)); } catch (e) {} }
  };
  var S = {
    gid: null, snap: null, games: [], team: '',
    queue: store.get('cs_queue', []), inflight: false, inflightAt: 0,
    offset: 0, online: false,
    kid: store.get('cs_kid', null), keeper: store.get('cs_keeper', ''),
    jobs: store.get('cs_jobs', { actions: true, shots: false, poss: false, clock: false, ball: false }),
    bt: null, btHow: 'dreb', btHist: [], btCids: null,
    view: store.get('cs_view', 'both'),
    kbd: store.get('cs_kbd', !window.matchMedia('(pointer: coarse)').matches),
    sel: { us: 'T', opp: 'T' }, out: { us: null, opp: null }, more: { us: false, opp: false },
    shot: null, local: {}, confirmDel: null, flash: null, clockBase: null, seq: 0
  };
  if (!S.kid) { S.kid = 'k' + Math.random().toString(36).slice(2, 9); store.set('cs_kid', S.kid); }

  // ---------- small helpers ----------
  function $(id) { return document.getElementById(id); }
  function esc(s) { return String(s == null ? '' : s).replace(/[&<>"']/g, function (c) { return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]; }); }
  function arr(v) { return Array.isArray(v) ? v : (v == null ? [] : [v]); }
  function rows(df) {                 // R data frames arrive as {col: [...]}; turn them into row objects
    if (!df) return [];
    if (Array.isArray(df)) return df;
    var keys = Object.keys(df); if (!keys.length) return [];
    var n = 0; keys.forEach(function (k) { n = Math.max(n, arr(df[k]).length); });
    var out = [];
    for (var i = 0; i < n; i++) { var r = {}; keys.forEach(function (k) { r[k] = arr(df[k])[i]; if (r[k] === undefined) r[k] = null; }); out.push(r); }
    return out;
  }
  function now() { return Date.now() + S.offset; }
  function other(t) { return t === 'us' ? 'opp' : 'us'; }
  function newCid() { S.seq += 1; return S.kid + '-' + Date.now().toString(36) + '-' + S.seq; }
  function toast(msg, warn) {
    var t = $('cs-toast'); if (!t) return;
    t.textContent = msg; t.className = 'cs-toast show' + (warn ? ' warn' : '');
    clearTimeout(toast._t); toast._t = setTimeout(function () { t.className = 'cs-toast'; }, warn ? 3500 : 1800);
  }

  // ---------- game data ----------
  function st() { var s = S.snap ? S.snap.state : {}; var o = {}; for (var k in s) o[k] = s[k]; for (var j in S.local) o[j] = S.local[j]; return o; }
  function roster(team) { return S.snap ? rows(S.snap.roster[team]) : []; }
  function pmap() { var m = {}; ['us', 'opp'].forEach(function (t) { roster(t).forEach(function (p) { m[p.id] = p; }); }); return m; }
  function plabel(pid) { if (pid == null || pid === 'T') return 'Team'; var p = pmap()[String(pid)]; return p ? '#' + p.num + (p.name ? ' ' + p.name : '') : '#?'; }
  function tname(t) { if (!S.snap) return t === 'us' ? 'Us' : 'Opponent'; return t === 'us' ? S.snap.game.team : S.snap.game.opponent; }
  function lineup(t) { var v = st()[t] || ''; return String(v).split(',').filter(function (x) { return x; }); }

  function events() {                 // server events + this device's unsent changes
    var m = {}, order = [];
    rows(S.snap ? S.snap.events : null).forEach(function (e) { m[e.cid] = e; order.push(e.cid); });
    S.queue.forEach(function (op) {
      if (op.gid !== S.gid) return;
      if (op.op === 'add') { if (!m[op.ev.cid]) order.push(op.ev.cid); var c = {}; for (var k in op.ev) c[k] = op.ev[k]; c.pending = true; m[op.ev.cid] = c; }
      else if (op.op === 'del') { delete m[op.cid]; for (var c2 in m) if (m[c2] && m[c2].linked_cid === op.cid) delete m[c2]; }
      else if (op.op === 'upd' && m[op.cid]) { var u = {}; for (var k2 in m[op.cid]) u[k2] = m[op.cid][k2]; for (var f in op.fields) u[f] = op.fields[f]; m[op.cid] = u; }
    });
    var out = order.filter(function (c) { return m[c]; }).map(function (c) { return m[c]; });
    out.sort(function (a, b) { return (a.ts || 0) - (b.ts || 0); });
    return out;
  }
  function pts(e) { return (e.type === 'FG' || e.type === 'FT') && Number(e.made) === 1 ? Number(e.points) || 0 : (e.type === 'ADJ' ? Number(e.points) || 0 : 0); }
  function score(t) { return events().filter(function (e) { return e.team === t; }).reduce(function (s, e) { return s + pts(e); }, 0); }

  // ---------- clock ----------
  function clockRem() {
    var s = st(); if (s.rem == null || s.rem < 0) return null;
    var base = S.clockBase || { rem: s.rem, at: performance.now(), run: s.run };
    return base.run ? Math.max(0, base.rem - (performance.now() - base.at) / 1000) : base.rem;
  }
  function fmtClock(r) { if (r == null) return ''; var m = Math.floor(r / 60), s = Math.floor(r % 60); return m + ':' + (s < 10 ? '0' : '') + s; }
  function periodSecs(p) {
    var g = S.snap ? S.snap.game : {}; var np = Number(g.periods) || 4;
    var pi = /^OT/.test(p) ? np + Number(p.slice(2)) : Number(p);
    if (pi <= np) return (Number(g.period_min) || 8) * 60;
    return /NFHS|High/.test(g.format || '') ? 240 : 300;
  }

  // ---------- recording ----------
  function save() { store.set('cs_queue', S.queue); }
  function push(op) { op.oid = op.oid || newCid(); op.gid = S.gid; S.queue.push(op); save(); render(); flushSoon(); }
  function addEv(ev) {
    var s = st();
    ev.cid = ev.cid || newCid(); ev.ts = ev.ts || now(); ev.period = ev.period || s.period || '1';
    ev.clock = fmtClock(clockRem()); ev.entered_by = S.keeper || 'Keeper';
    if (s.track) { ev.on_court = lineup('us').join(','); ev.opp_on_court = lineup('opp').join(','); }
    push({ op: 'add', ev: ev });
    if (S.btCids) S.btCids.push(ev.cid);
    return ev.cid;
  }
  function recent(pred, secs) {
    var cut = now() - (secs || 12) * 1000, ev = events();
    for (var i = ev.length - 1; i >= 0; i--) { if (ev[i].ts < cut) break; if (pred(ev[i], ev)) return ev[i]; }
    return null;
  }
  function kids(cid, ev) { return (ev || events()).filter(function (e) { return e.linked_cid === cid; }); }

  var LABEL = { FG: 'FG', FT: 'FT', OREB: 'off rebound', DREB: 'def rebound', AST: 'assist', AST_ATT: 'assist attempt',
    STL: 'steal', BLK: 'block', DAST: 'def assist', TO: 'turnover', PF: 'foul', FD: 'fouled', CHG: 'charge taken',
    OUTLET: 'outlet', TRANS: 'fast break', TIMEOUT: 'timeout', JUMP: 'gets the jump ball', ADJ: 'score fix',
    POSS: 'ball', SHOTLOC: 'shot spot' };
  function describe(e) {
    var who = e.player_id == null ? tname(e.team) : plabel(e.player_id);
    var what = LABEL[e.type] || e.type;
    if (e.type === 'FG') what = e.points + 'PT ' + (Number(e.made) === 1 ? 'made' : 'miss');
    if (e.type === 'FT') what = 'FT ' + (Number(e.made) === 1 ? 'made' : 'miss');
    if (e.type === 'ADJ') what = 'score fix ' + (e.points > 0 ? '+' : '') + e.points;
    if (e.type === 'POSS') { who = tname(e.team); what = 'ball (' + (POSS_LBL[e.tag] || e.tag) + ')'; }
    return who + ' - ' + what;
  }

  // Player first, then action. Rebounds work out offense/defense from the last miss;
  // the other team's half of a play (a steal's turnover, a drawn foul's foul) is added
  // automatically and gets the player if someone taps it within a few seconds.
  function act(team, code) {
    if (!S.gid) return toast('Pick a game first.', true);
    var sel = S.sel[team], pid = sel === 'T' ? null : sel, o = other(team), e;
    var who = pid ? plabel(pid) : tname(team);
    switch (code) {
      case '2m': case '2x': case '3m': case '3x':
        addEv({ team: team, type: 'FG', player_id: pid, points: Number(code[0]), made: code[1] === 'm' ? 1 : 0 }); break;
      case 'fm': case 'fx': addEv({ team: team, type: 'FT', player_id: pid, points: 1, made: code === 'fm' ? 1 : 0 }); break;
      case 'reb': case 'oreb': case 'dreb': {
        var miss = recent(function (x, ev) { return (x.type === 'FG' || x.type === 'FT') && Number(x.made) === 0 &&
          !kids(x.cid, ev).some(function (k) { return k.type === 'OREB' || k.type === 'DREB'; }); }, 15);
        var type = code === 'oreb' ? 'OREB' : code === 'dreb' ? 'DREB' : (miss && miss.team === team ? 'OREB' : 'DREB');
        addEv({ team: team, type: type, player_id: pid, linked_cid: miss ? miss.cid : null }); break; }
      case 'ast': {
        var mk = recent(function (x, ev) { return x.type === 'FG' && x.team === team && Number(x.made) === 1 &&
          !kids(x.cid, ev).some(function (k) { return k.type === 'AST'; }); }, 12);
        addEv({ team: team, type: 'AST', player_id: pid, linked_cid: mk ? mk.cid : null }); break; }
      case 'aa': {
        var ms = recent(function (x) { return x.type === 'FG' && x.team === team && Number(x.made) === 0; }, 12);
        addEv({ team: team, type: 'AST_ATT', player_id: pid, linked_cid: ms ? ms.cid : null }); break; }
      case 'blk': {
        var bm = recent(function (x) { return x.type === 'FG' && x.team === o && Number(x.made) === 0; }, 12);
        addEv({ team: team, type: 'BLK', player_id: pid, linked_cid: bm ? bm.cid : null }); break; }
      case 'stl': {
        var to = recent(function (x, ev) { return x.type === 'TO' && x.team === o && !x.linked_cid &&
          !kids(x.cid, ev).some(function (k) { return k.type === 'STL'; }); }, 12);
        if (to) { addEv({ team: team, type: 'STL', player_id: pid, linked_cid: to.cid }); break; }
        var sc = addEv({ team: team, type: 'STL', player_id: pid });
        addEv({ team: o, type: 'TO', player_id: null, linked_cid: sc, tag: 'auto' }); break; }
      case 'to': {
        var auto = recent(function (x) { return x.type === 'TO' && x.team === team && x.tag === 'auto' && x.player_id == null; }, 15);
        if (auto) { push({ op: 'upd', cid: auto.cid, fields: { player_id: pid, tag: null } }); break; }
        addEv({ team: team, type: 'TO', player_id: pid }); break; }
      case 'fd': {
        var pf = recent(function (x, ev) { return x.type === 'PF' && x.team === o && !x.linked_cid &&
          !kids(x.cid, ev).some(function (k) { return k.type === 'FD'; }); }, 12);
        if (pf) { addEv({ team: team, type: 'FD', player_id: pid, linked_cid: pf.cid }); break; }
        var fc = addEv({ team: team, type: 'FD', player_id: pid });
        addEv({ team: o, type: 'PF', player_id: null, linked_cid: fc, tag: 'auto' }); break; }
      case 'pf': {
        var apf = recent(function (x) { return x.type === 'PF' && x.team === team && x.tag === 'auto' && x.player_id == null; }, 15);
        if (apf) { push({ op: 'upd', cid: apf.cid, fields: { player_id: pid, tag: null } }); break; }
        addEv({ team: team, type: 'PF', player_id: pid }); break; }
      case 'chg': {
        var cc = addEv({ team: team, type: 'CHG', player_id: pid });
        addEv({ team: o, type: 'TO', player_id: null, linked_cid: cc, tag: 'auto' });
        addEv({ team: o, type: 'PF', player_id: null, linked_cid: cc, tag: 'auto' }); break; }
      case 'da': {
        var s2 = recent(function (x) { return x.type === 'STL' && x.team === team; }, 12);
        addEv({ team: team, type: 'DAST', player_id: pid, linked_cid: s2 ? s2.cid : null }); break; }
      case 'out': {
        var dr = recent(function (x) { return x.type === 'DREB' && x.team === team; }, 12);
        addEv({ team: team, type: 'OUTLET', player_id: pid, linked_cid: dr ? dr.cid : null }); break; }
      case 'fb': addEv({ team: team, type: 'TRANS', player_id: pid }); break;
      default: return;
    }
    S.flash = team + ':' + code;
    toast(who + ' - ' + (ACTION_NAME[code] || code));
  }
  var ACTION_NAME = { '2m': 'made 2', '2x': 'missed 2', '3m': 'made 3', '3x': 'missed 3', 'fm': 'made FT', 'fx': 'missed FT',
    reb: 'rebound', oreb: 'off rebound', dreb: 'def rebound', ast: 'assist', aa: 'assist attempt', blk: 'block', stl: 'steal',
    to: 'turnover', fd: 'fouled', pf: 'foul', chg: 'charge taken', da: 'def assist', out: 'outlet', fb: 'fast break' };
  var MAIN_ACTS = [['2m', '2 ✓', 'mk'], ['2x', '2 ✗', 'ms'], ['3m', '3 ✓', 'mk'], ['3x', '3 ✗', 'ms'],
    ['fm', 'FT ✓', 'mk'], ['fx', 'FT ✗', 'ms'], ['reb', 'Reb', ''], ['ast', 'Ast', ''], ['to', 'TO', 'bad'],
    ['stl', 'Stl', ''], ['pf', 'Foul', 'bad'], ['fd', 'Fouled', ''], ['blk', 'Blk', '']];
  var MORE_ACTS = [['chg', 'Charge'], ['aa', 'Ast att'], ['da', 'Def ast'], ['out', 'Outlet'], ['fb', 'Fast brk'],
    ['oreb', 'Off reb'], ['dreb', 'Def reb']];

  function undoMine() {
    var mine = events().filter(function (e) { return (e.session_id === S.kid || e.pending) && !e.linked_cid; });
    if (!mine.length) return toast('Nothing of yours to undo.', true);
    var last = mine[mine.length - 1]; push({ op: 'del', cid: last.cid }); toast('Undid: ' + describe(last));
  }

  // ---------- keyboard codes: 12+2 made 2, 12-3 missed 3, 12+1 made FT, 12r, o24f, tr (team rebound) ----------
  var KEYCODE = { '+2': '2m', '-2': '2x', '+3': '3m', '-3': '3x', '+1': 'fm', '-1': 'fx', r: 'reb', a: 'ast', t: 'to',
    s: 'stl', b: 'blk', f: 'pf', d: 'fd', c: 'chg', q: 'aa', x: 'da', p: 'out', k: 'fb' };
  function parseCmd(txt) {
    var m = String(txt).trim().toLowerCase().replace(/\s+/g, '').match(/^(o)?(t|\d{1,3})([+-][123]|[a-z])$/);
    if (!m) return null;
    var team = m[1] ? 'opp' : 'us', code = KEYCODE[m[3]]; if (!code) return null;
    var pid = 'T';
    if (m[2] !== 't') { var p = roster(team).filter(function (r) { return String(r.num) === m[2]; })[0];
      if (!p) return { error: 'No #' + m[2] + ' on ' + tname(team) + ' (add the player below the list).' }; pid = p.id; }
    return { team: team, pid: pid, code: code };
  }
  function runCmd() {
    var box = $('cs-cmd'); if (!box || !box.value.trim()) return;
    var c = parseCmd(box.value);
    if (!c) return toast('Not a code. Examples: 12+2, 12-3, 5r, o24f, tr', true);
    if (c.error) return toast(c.error, true);
    S.sel[c.team] = c.pid; act(c.team, c.code); box.value = ''; render();
  }

  // ---------- possessions ----------
  var POSS_LBL = { dreb: 'def rebound', to: 'steal / turnover', made: 'after a score', ft: 'after free throws',
    deadball: 'dead ball / inbound', jump: 'jump ball', period: 'start of period' };
  function curPoss() { var p = events().filter(function (e) { return e.type === 'POSS'; }); return p.length ? p[p.length - 1] : null; }

  // ---------- shot spots (full court, feet: X 0..94 along, Y 0..50 across) ----------
  function usLeftNow() {
    var s = st(), g = S.snap ? S.snap.game : {}, np = Number(g.periods) || 4, p = s.period || '1';
    var pi = /^OT/.test(p) ? np + Number(p.slice(2)) : Number(p);
    return (!!s.us_left) !== (pi > np / 2);
  }
  function toHalf(X, Y) { return X < 47 ? { side: 'left', x: Y - 25, y: X } : { side: 'right', x: 25 - Y, y: 94 - X }; }
  function toFull(x, y, side) { return side === 'left' ? { X: y, Y: x + 25 } : { X: 94 - y, Y: 25 - x }; }
  function courtSVG() {
    var g = S.snap ? S.snap.game : {}, r = Number(g.three) || 19.75, c = Math.min(Number(g.corner) || 19.75, r), lane = Number(g.lane) || 12;
    var th = c < r ? Math.acos(c / r) : 0, yc = 5.25 + r * Math.sin(th);
    function end(side) {
      function P(x, y) { var f = toFull(x, y, side); return (f.X * 10).toFixed(1) + ',' + ((50 - f.Y) * 10).toFixed(1); }
      function arc(cx, cy, rad, a0, a1, n) { var pts = []; for (var i = 0; i <= n; i++) { var a = a0 + (a1 - a0) * i / n; pts.push(P(cx + rad * Math.cos(a), cy + rad * Math.sin(a))); } return pts.join(' '); }
      return '<polygon class="lane" points="' + [P(-lane / 2, 0), P(-lane / 2, 19), P(lane / 2, 19), P(lane / 2, 0)].join(' ') + '"/>' +
        '<polyline points="' + arc(0, 19, 6, 0, Math.PI, 30) + '"/>' +
        '<polyline points="' + arc(0, 5.25, 4, 0, Math.PI, 20) + '"/>' +
        '<polyline class="arc" points="' + P(c, 0) + ' ' + arc(0, 5.25, r, th, Math.PI - th, 60) + ' ' + P(-c, 0) + '"/>' +
        '<polyline class="bb" points="' + P(-3, 4) + ' ' + P(3, 4) + '"/>' +
        '<polyline class="rim" points="' + arc(0, 5.25, 0.75, 0, 2 * Math.PI, 16) + '"/>';
    }
    var ul = usLeftNow();
    var dots = events().filter(function (e) { return e.type === 'SHOTLOC' && e.x != null; }).slice(-14).map(function (e) {
      var leftTeam = ul ? 'us' : 'opp', side = e.team === leftTeam ? 'left' : 'right', f = toFull(Number(e.x), Number(e.y), side);
      return '<circle class="dot ' + e.team + '" cx="' + f.X * 10 + '" cy="' + (50 - f.Y) * 10 + '" r="9"/>';
    }).join('');
    var pend = S.shot ? '<circle class="pend" cx="' + S.shot.X * 10 + '" cy="' + (50 - S.shot.Y) * 10 + '" r="12"/>' : '';
    return '<svg id="cs-court" viewBox="0 0 940 500" preserveAspectRatio="xMidYMid meet">' +
      '<rect class="floor" x="0" y="0" width="940" height="500"/><line x1="470" y1="0" x2="470" y2="500"/>' +
      '<circle cx="470" cy="250" r="60" class="ln"/>' + end('left') + end('right') +
      '<text x="235" y="30" class="lbl ' + (ul ? 'us' : 'opp') + '">' + esc(tname(ul ? 'us' : 'opp')) + ' shoots here</text>' +
      '<text x="705" y="30" class="lbl ' + (ul ? 'opp' : 'us') + '">' + esc(tname(ul ? 'opp' : 'us')) + ' shoots here</text>' +
      dots + pend + '</svg>';
  }

  // ---------- ball tracker (our team): one tap per pass ----------
  var BT_TYPES = ['PASS', 'TOUCH', 'DRIVE', 'POST'];
  function btActive() {
    if (S.jobs.ball) return true;
    var cut = now() - 180000;
    return events().some(function (e) { return BT_TYPES.indexOf(e.type) >= 0 && e.ts > cut; });
  }
  function btKey() { return 'cs_bt_' + S.gid; }
  function bt() { if (!S.bt || S.bt.gid !== S.gid) S.bt = store.get(btKey(), { gid: S.gid, state: 'wait' }); return S.bt; }
  function btSet(patch) { var b = bt(); for (var k in patch) b[k] = patch[k]; store.set(btKey(), b); }
  function likelyFive() {
    var seen = [], ev = events();
    for (var i = ev.length - 1; i >= 0 && seen.length < 5; i--) {
      var e = ev[i], ids = [];
      if (e.team === 'us' && e.player_id != null) ids.push(String(e.player_id));
      if (e.type === 'PASS' && e.target_id != null) ids.push(String(e.target_id));
      ids.forEach(function (x) { if (seen.indexOf(x) < 0 && seen.length < 5) seen.push(x); });
    }
    return seen;
  }
  function btDo(fn) {                         // one tap = one undoable step (may add several plays)
    var before = JSON.parse(JSON.stringify(bt())); S.btCids = [];
    fn();
    S.btHist.push({ state: before, cids: S.btCids }); if (S.btHist.length > 30) S.btHist.shift();
    S.btCids = null; render();
  }
  function btUndo() {
    var h = S.btHist.pop(); if (!h) return toast('Nothing to undo here.', true);
    h.cids.forEach(function (c) { push({ op: 'del', cid: c }); }); S.bt = h.state; store.set(btKey(), S.bt);
    toast('Undone'); render();
  }
  function btTapPlayer(id) {
    var b = bt();
    if (b.state === 'line') return toast('Record the free throws first.', true);
    if (b.state === 'ball') {
      if (String(id) === String(b.holder)) return;
      btDo(function () { addEv({ team: 'us', type: 'PASS', player_id: b.holder, target_id: id });
        btSet({ passFrom: b.holder, holder: id, since: now(), drive: false }); });
      return;
    }
    if (b.state === 'reb') {
      btDo(function () { addEv({ team: 'us', type: 'OREB', player_id: id, linked_cid: b.shot || null });
        btSet({ state: 'ball', holder: id, since: now(), passFrom: null, drive: false }); });
      toast(plabel(id) + ' - off rebound'); return;
    }
    var how = b.state === 'inbound' ? 'inbound' : S.btHow;     // their ball / dead ball: she gets it
    btDo(function () {
      if (how === 'dreb') {
        var miss = recent(function (x, ev) { return x.team === 'opp' && (x.type === 'FG' || x.type === 'FT') && Number(x.made) === 0 &&
          !kids(x.cid, ev).some(function (k) { return k.type === 'OREB' || k.type === 'DREB'; }); }, 20);
        addEv({ team: 'us', type: 'DREB', player_id: id, linked_cid: miss ? miss.cid : null });
      } else if (how === 'steal') {
        var sc = addEv({ team: 'us', type: 'STL', player_id: id });
        addEv({ team: 'opp', type: 'TO', player_id: null, linked_cid: sc, tag: 'auto' });
      } else addEv({ team: 'us', type: 'TOUCH', player_id: id, tag: 'inbound' });
      btSet({ state: 'ball', holder: id, since: now(), passFrom: null, drive: false });
    });
    S.btHow = 'dreb';
    toast(plabel(id) + ' has the ball (' + (how === 'dreb' ? 'def rebound' : how) + ')');
  }
  function btAct(code) {
    var b = bt(), h = b.holder, quick = b.passFrom && (now() - (b.since || 0)) <= 3000;
    if (code === 'their') { btDo(function () { btSet({ state: 'wait', holder: null }); }); return; }
    if (code === 'ours') { btDo(function () { btSet({ state: 'inbound', holder: null }); }); return; }
    if (b.state === 'reb' && code === 'dreb_them') {
      btDo(function () { addEv({ team: 'opp', type: 'DREB', player_id: null, linked_cid: b.shot || null }); btSet({ state: 'wait', holder: null }); }); return; }
    if (b.state === 'line') {
      if (code === 'ftm' || code === 'ftx') { btDo(function () { var c = addEv({ team: 'us', type: 'FT', player_id: b.shooter, points: 1, made: code === 'ftm' ? 1 : 0 });
        btSet({ shot: c }); }); return; }
      if (code === 'ftreb') { btDo(function () { btSet({ state: 'reb' }); }); return; }
    }
    if (b.state !== 'ball' || !h) return toast('Tap who has the ball first.', true);
    btDo(function () {
      if (code === 'drive' || code === 'post') { addEv({ team: 'us', type: code === 'drive' ? 'DRIVE' : 'POST', player_id: h }); return; }
      if (/^[23][mx]$/.test(code)) {
        var made = code[1] === 'm', c = addEv({ team: 'us', type: 'FG', player_id: h, points: Number(code[0]), made: made ? 1 : 0 });
        if (quick) addEv({ team: 'us', type: made ? 'AST' : 'AST_ATT', player_id: b.passFrom, linked_cid: c, tag: 'auto' });
        btSet(made ? { state: 'wait', holder: null } : { state: 'reb', holder: null, shot: c }); return;
      }
      if (code === 'to') { addEv({ team: 'us', type: 'TO', player_id: h }); btSet({ state: 'wait', holder: null }); return; }
      if (code === 'fds' || code === 'fdn') {
        var f = addEv({ team: 'us', type: 'FD', player_id: h }); addEv({ team: 'opp', type: 'PF', player_id: null, linked_cid: f, tag: 'auto' });
        btSet(code === 'fds' ? { state: 'line', shooter: h, holder: null } : { state: 'inbound', holder: null }); return;
      }
    });
    toast(plabel(h) + ' - ' + ({ drive: 'drives', post: 'posts up', '2m': 'made 2', '3m': 'made 3', '2x': 'missed 2', '3x': 'missed 3',
      to: 'turnover', fds: 'fouled shooting', fdn: 'fouled' }[code] || code));
  }
  function renderBall() {
    var el = $('cs-ball'); if (!S.jobs.ball || !S.snap) { el.innerHTML = ''; return; }
    var b = bt(), five = likelyFive(), rs = roster('us');
    var status = { wait: 'Their ball. When ' + esc(tname('us')) + ' gets it, tap who has it.',
      inbound: esc(tname('us')) + ' ball (inbound). Tap who gets it.',
      ball: 'Ball: <b>' + esc(plabel(b.holder)) + '</b> &middot; <span id="cs-bt-t">0.0</span>s',
      reb: 'Missed. Tap who got the offensive rebound, or "Them".',
      line: esc(plabel(b.shooter)) + ' at the line.' }[b.state] || '';
    var hows = b.state === 'wait' ? '<div class="bthow">How: ' + [['dreb', 'Def rebound'], ['steal', 'Steal'], ['inbound', 'Inbound']].map(function (o) {
      return '<button class="chip sm' + (S.btHow === o[0] ? ' on' : '') + '" data-bthow="' + o[0] + '">' + o[1] + '</button>'; }).join('') + '</div>' : '';
    var grid = rs.map(function (p) {
      var id = String(p.id), cls = String(b.holder) === id ? ' holder' : five.indexOf(id) >= 0 ? ' likely' : '';
      return '<button class="btp' + cls + '" data-btp="' + id + '"' + (b.state === 'line' ? ' disabled' : '') + '><b>' + esc(p.num) + '</b><span>' + esc(p.name) + '</span></button>';
    }).join('');
    var A = function (c, l, k) { return '<button class="bta ' + (k || '') + '" data-bta="' + c + '">' + l + '</button>'; };
    var acts = b.state === 'ball' ? A('drive', 'Drives') + A('post', 'Posts up') + A('2m', '2 ✓', 'mk') + A('3m', '3 ✓', 'mk') +
        A('2x', '2 ✗') + A('3x', '3 ✗') + A('to', 'Turnover', 'bad') + A('fds', 'Fouled shooting') + A('fdn', 'Fouled') + A('their', 'Their ball')
      : b.state === 'reb' ? A('dreb_them', 'Them (def reb)', 'bad') + A('their', 'Their ball')
      : b.state === 'line' ? A('ftm', 'FT ✓', 'mk') + A('ftx', 'FT ✗') + A('ftreb', 'Live rebound') + A('their', 'Their ball')
      : A('ours', 'Our ball (inbound)');
    el.innerHTML = '<div class="ph"><b>Ball tracker</b> <span class="hint">' + esc(tname('us')) +
      ': tap the player who gets each pass. Green = likely on the court.</span> <button class="chip sm warnb" data-btundo="1">Undo</button></div>' +
      '<div class="btstatus ' + b.state + '">' + status + '</div>' + hows + '<div class="btgrid">' + grid + '</div><div class="btacts">' + acts + '</div>';
  }

  // ---------- rendering ----------
  function teamFouls(t) {
    var s = st(), g = S.snap ? S.snap.game : {}, np = Number(g.periods) || 4, p = s.period || '1';
    var half = function (q) { var pi = /^OT/.test(q) ? np + 1 : Number(q); return pi > np / 2; };
    return events().filter(function (e) { return e.type === 'PF' && e.team === t &&
      (/College Men/.test(g.format || '') ? half(e.period) === half(p) : e.period === p); }).length;
  }
  function bonus(t) {
    var f = teamFouls(other(t)), g = S.snap ? S.snap.game : {};
    if (/College Men/.test(g.format || '')) return f >= 9 ? 'DOUBLE BONUS' : f >= 6 ? 'BONUS' : '';
    return f >= 4 ? 'BONUS' : '';
  }
  function arrowTeam() { var j = events().filter(function (e) { return e.type === 'JUMP'; }); return j.length ? other(j[j.length - 1].team) : null; }

  function renderTop() {
    var waiting = S.queue.length, cls = !S.online ? 'off' : waiting ? 'wait' : 'ok';
    var txt = !S.online ? 'Offline - ' + waiting + ' play' + (waiting === 1 ? '' : 's') + ' saved on this device' :
      waiting ? 'Sending ' + waiting + '...' : 'All plays saved';
    var opts = S.games.map(function (g) { return '<option value="' + g.id + '"' + (String(g.id) === String(S.gid) ? ' selected' : '') + '>' + esc(g.label) + '</option>'; }).join('');
    var job = function (k, l) { return '<button class="chip' + (S.jobs[k] ? ' on' : '') + '" data-job="' + k + '">' + l + '</button>'; };
    $('cs-top').innerHTML =
      '<select id="cs-game"><option value="">Pick a game...</option>' + opts + '</select>' +
      '<input id="cs-name" placeholder="Your name" value="' + esc(S.keeper) + '">' +
      '<span class="lbl">My jobs:</span>' + job('actions', 'Actions') + job('shots', 'Shot spots') + job('poss', 'Possessions') + job('clock', 'Clock &amp; subs') + job('ball', 'Ball tracker') +
      '<span class="sync ' + cls + '">' + txt + '</span>';
  }
  function renderSB() {
    if (!S.snap) { $('cs-sb').innerHTML = '<div class="cs-empty">Pick a game above. New games are created on the Classic Entry tab.</div>'; return; }
    var s = st(), a = arrowTeam();
    var side = function (t) {
      var to = events().filter(function (e) { return e.type === 'TIMEOUT' && e.team === t; }).length, b = bonus(t);
      return '<div class="sb-side ' + t + '"><div class="nm">' + esc(tname(t)) + '</div><div class="sc">' + score(t) + '</div>' +
        '<div class="inf">Fouls ' + teamFouls(t) + ' &middot; TO ' + to + (b ? ' <b class="bonus">' + b + '</b>' : '') + '</div></div>';
    };
    $('cs-sb').innerHTML = side('us') + '<div class="sb-mid"><div class="per">' + (/^OT/.test(s.period) ? s.period : (Number(S.snap.game.periods) === 2 ? 'Half ' : 'Q') + s.period) + '</div>' +
      '<div id="cs-clock" class="clk">--:--</div><div class="arrow">' + (a ? 'Arrow → ' + esc(tname(a)) : 'Arrow: opening tip') + '</div></div>' + side('opp');
  }
  function teamBlock(t) {
    var sel = S.sel[t], rs = roster(t), lu = lineup(t), track = st().track;
    var list = '<div class="pl' + (sel === 'T' ? ' sel' : '') + '" data-pick="' + t + '|T"><span class="n">TEAM</span></div>' +
      rs.map(function (p) { var on = track && lu.indexOf(String(p.id)) >= 0;
        return '<div class="pl' + (String(sel) === String(p.id) ? ' sel' : '') + (on ? ' oncourt' : '') + '" data-pick="' + t + '|' + p.id + '">' +
          '<span class="n">' + esc(p.num) + '</span><span class="nm">' + esc(p.name) + '</span></div>'; }).join('');
    var btn = function (a) { return '<button class="ab ' + (a[2] || '') + (S.flash === t + ':' + a[0] ? ' flash' : '') + '" data-act="' + t + '|' + a[0] + '">' + a[1] + '</button>'; };
    var bt = t === 'us' && btActive();
    var main = bt ? MAIN_ACTS.filter(function (a) { return ['pf', 'blk'].indexOf(a[0]) >= 0; }) : MAIN_ACTS;
    var more = bt ? MORE_ACTS.filter(function (a) { return ['chg', 'da'].indexOf(a[0]) >= 0; }) : MORE_ACTS;
    var acts = (bt ? '<div class="btnote">Ball tracker is recording ' + esc(tname('us')) + ' shots, free throws, rebounds, steals, turnovers and assists.</div>' : '') +
      main.map(btn).join('') + '<button class="ab more" data-more="' + t + '">' + (S.more[t] ? 'Less' : 'More') + '</button>' +
      (S.more[t] ? more.map(btn).join('') : '');
    return '<div class="tb ' + t + '"><div class="th">' + esc(tname(t)) + '<span class="who">' + esc(plabel(sel === 'T' ? null : sel)) + '</span></div>' +
      '<div class="tbody"><div class="plist">' + list + '</div><div class="acts">' + acts + '</div></div>' +
      '<div class="addp"><input placeholder="Add player: 23 Smith" data-addp="' + t + '"><button data-addbtn="' + t + '">Add</button></div></div>';
  }
  function renderActions() {
    var el = $('cs-actions'); if (!S.jobs.actions || !S.snap) { el.innerHTML = ''; return; }
    var v = S.view, ev = events().slice(-8).reverse();
    var vbtn = function (k, l) { return '<button class="chip sm' + (v === k ? ' on' : '') + '" data-view="' + k + '">' + l + '</button>'; };
    el.innerHTML = '<div class="ph"><b>Actions</b> <span class="hint">Tap a player, then an action.</span> ' +
      vbtn('both', 'Both teams') + vbtn('us', esc(tname('us'))) + vbtn('opp', esc(tname('opp'))) +
      ' <button class="chip sm' + (S.kbd ? ' on' : '') + '" data-kbd="1">Keyboard</button>' +
      ' <button class="chip sm warnb" data-undo="1">Undo my last</button></div>' +
      (S.kbd ? '<div class="cmd"><input id="cs-cmd" autocomplete="off" placeholder="Type a code and press Enter: 12+2  12-3  12+1  5r  3t  o24f  tr"><span class="hint">+2/-2 made/missed 2 &middot; +3/-3 &middot; +1/-1 FT &middot; r reb &middot; a ast &middot; t TO &middot; s stl &middot; b blk &middot; f foul &middot; d fouled &middot; c charge &middot; o = opponent &middot; t = team</span></div>' : '') +
      '<div class="teams">' + (v !== 'opp' ? teamBlock('us') : '') + (v !== 'us' ? teamBlock('opp') : '') + '</div>' +
      '<div class="recent"><b>Recent plays</b>' + ev.map(function (e) {
        var c = S.confirmDel === e.cid;
        return '<div class="rp ' + e.team + (e.pending ? ' pend' : '') + '"><span>' + esc(e.clock || '') + ' ' + esc(describe(e)) +
          (e.pending ? ' <i>(sending)</i>' : '') + '</span><button class="x' + (c ? ' really' : '') + '" data-del="' + esc(e.cid) + '">' + (c ? 'Delete?' : '✕') + '</button></div>';
      }).join('') + '</div>';
    var box = $('cs-cmd'); if (box && S.kbd && !window.matchMedia('(pointer: coarse)').matches && S._focusCmd) { box.focus(); }
  }
  function renderShots() {
    var el = $('cs-shots'); if (!S.jobs.shots || !S.snap) { el.innerHTML = ''; return; }
    var pad = '';
    if (S.shot) {
      pad = '<div class="pad"><b>Who shot for ' + esc(tname(S.shot.team)) + '?</b> ' +
        roster(S.shot.team).map(function (p) { return '<button class="num" data-shooter="' + p.id + '">' + esc(p.num) + '</button>'; }).join('') +
        '<button class="num" data-shooter="T">?</button><button class="num cancel" data-shooter="X">Cancel</button></div>';
    }
    el.innerHTML = '<div class="ph"><b>Shot spots</b> <span class="hint">Tap where the shot was taken, then the shooter\'s number. Made/missed comes from the scorer.</span> ' +
      '<button class="chip sm" data-sides="1">Switch baskets</button></div>' + courtSVG() + pad;
  }
  function renderPoss() {
    var el = $('cs-poss'); if (!S.jobs.poss || !S.snap) { el.innerHTML = ''; return; }
    var cp = curPoss(), ps = events().filter(function (e) { return e.type === 'POSS'; });
    var row = function (t) {
      return '<div class="prow ' + t + '"><span class="pt">' + esc(tname(t)) + ' ball:</span>' +
        ['dreb', 'to', 'made', 'ft', 'deadball', 'jump'].map(function (k) { return '<button class="pb" data-poss="' + t + '|' + k + '">' + POSS_LBL[k] + '</button>'; }).join('') + '</div>';
    };
    var cur = cp ? '<span class="cur ' + cp.team + '">' + esc(tname(cp.team)) + ' ball &middot; ' + POSS_LBL[cp.tag] + ' &middot; <span id="cs-pt">0</span>s</span>' : '<span class="cur">No possession yet - tap who gets the opening tip</span>';
    var cnt = function (t) { return ps.filter(function (e) { return e.team === t; }).length; };
    el.innerHTML = '<div class="ph"><b>Possessions</b> <span class="hint">Tap each time the ball changes hands, and how.</span></div>' +
      '<div class="pcur">' + cur + (cp ? ' <button class="pb fb" data-fb="1">Fast break!</button> <button class="chip sm warnb" data-delposs="1">Undo last</button>' : '') + '</div>' +
      row('us') + row('opp') +
      '<div class="hint">Possessions: ' + esc(tname('us')) + ' ' + cnt('us') + ' &middot; ' + esc(tname('opp')) + ' ' + cnt('opp') + '</div>';
  }
  function renderClockPanel() {
    var el = $('cs-clockp'); if (!S.jobs.clock || !S.snap) { el.innerHTML = ''; return; }
    var s = st(), g = S.snap.game, np = Number(g.periods) || 4, pers = [];
    for (var i = 1; i <= np; i++) pers.push(String(i)); pers.push('OT1', 'OT2', 'OT3');
    var a = arrowTeam();
    var lineupBlock = function (t) {
      var lu = lineup(t), rs = roster(t), out = S.out[t];
      var tile = function (p, on) { return '<div class="tile' + (on ? ' on' : ' bench') + (out === String(p.id) ? ' outp' : '') + '" data-sub="' + t + '|' + p.id + '"><b>' + esc(p.num) + '</b><span>' + esc(p.name) + '</span></div>'; };
      var onr = lu.map(function (id) { return rs.filter(function (p) { return String(p.id) === id; })[0]; }).filter(Boolean);
      return '<div class="lu ' + t + '"><div class="th">' + esc(tname(t)) + ' on court (' + lu.length + ')' + '</div>' +
        '<div class="tiles">' + onr.map(function (p) { return tile(p, true); }).join('') + '</div><div class="tiles">' +
        rs.filter(function (p) { return lu.indexOf(String(p.id)) < 0; }).map(function (p) { return tile(p, false); }).join('') + '</div></div>';
    };
    el.innerHTML = '<div class="ph"><b>Clock &amp; subs</b> <span class="hint">Space bar starts/stops the clock (on a keyboard).</span></div>' +
      '<div class="crow"><button id="cs-clockbtn" class="big ' + (s.run ? 'stop' : 'go') + '" data-clock="toggle">' + (s.run ? 'Stop clock' : 'Start clock') + '</button>' +
      '<button class="mid" data-clock="-1">-1s</button><button class="mid" data-clock="1">+1s</button>' +
      '<input id="cs-setclk" placeholder="m:ss"><button class="mid" data-clock="set">Set</button>' +
      '<select id="cs-period">' + pers.map(function (p) { return '<option' + (p === s.period ? ' selected' : '') + '>' + p + '</option>'; }).join('') + '</select></div>' +
      '<div class="crow"><button class="mid" data-timeout="us">' + esc(tname('us')) + ' timeout</button><button class="mid" data-timeout="opp">' + esc(tname('opp')) + ' timeout</button>' +
      '<span class="hint">Jump ball / held ball' + (a ? ' (arrow → ' + esc(tname(a)) + ')' : '') + ':</span>' +
      '<button class="mid" data-jump="us">→ ' + esc(tname('us')) + '</button><button class="mid" data-jump="opp">→ ' + esc(tname('opp')) + '</button></div>' +
      '<div class="crow"><span class="hint">Fix score:</span><input id="cs-fix-us" type="number" value="' + score('us') + '"><span>-</span><input id="cs-fix-opp" type="number" value="' + score('opp') + '">' +
      '<button class="mid" data-fix="1">Apply</button>' +
      '<label class="tk"><input type="checkbox" id="cs-track"' + (s.track ? ' checked' : '') + '> Track who\'s on the court (for +/- and lineups)</label></div>' +
      (s.track ? '<div class="hint">Subs: tap the player coming out (turns red), then the player going in. Tap a red player again to bench her.</div>' +
        '<div class="lus">' + lineupBlock('us') + lineupBlock('opp') + '</div>' : '');
  }
  function render() {
    if (!$('cs-top')) return;
    renderTop(); renderSB(); renderClockPanel(); renderBall(); renderActions(); renderShots(); renderPoss();
    S.flash = null;
  }

  // ---------- events from the page ----------
  function onClick(ev) {
    var t = ev.target.closest('[data-btp],[data-bta],[data-bthow],[data-btundo],[data-pick],[data-act],[data-more],[data-job],[data-view],[data-kbd],[data-undo],[data-del],[data-shooter],[data-sides],[data-poss],[data-fb],[data-delposs],[data-clock],[data-timeout],[data-jump],[data-fix],[data-sub],[data-addbtn]');
    if (!t) return;
    var d = t.dataset;
    if (d.del === undefined && S.confirmDel) S.confirmDel = null;
    if (d.btp) { btTapPlayer(d.btp); return render(); }
    if (d.bta) { btAct(d.bta); return render(); }
    if (d.bthow) { S.btHow = d.bthow; return render(); }
    if (d.btundo) { btUndo(); return; }
    if (d.pick) { var p = d.pick.split('|'); S.sel[p[0]] = p[1]; S._focusCmd = false; return render(); }
    if (d.act) { var a = d.act.split('|'); act(a[0], a[1]); return render(); }
    if (d.more) { S.more[d.more] = !S.more[d.more]; return render(); }
    if (d.job) { S.jobs[d.job] = !S.jobs[d.job]; store.set('cs_jobs', S.jobs); return render(); }
    if (d.view) { S.view = d.view; store.set('cs_view', S.view); return render(); }
    if (d.kbd) { S.kbd = !S.kbd; store.set('cs_kbd', S.kbd); S._focusCmd = S.kbd; return render(); }
    if (d.undo) { undoMine(); return render(); }
    if (d.del) { if (S.confirmDel === d.del) { push({ op: 'del', cid: d.del }); S.confirmDel = null; toast('Deleted'); } else S.confirmDel = d.del; return render(); }
    if (d.shooter) {
      if (d.shooter !== 'X' && S.shot) { addEv({ team: S.shot.team, type: 'SHOTLOC', player_id: d.shooter === 'T' ? null : d.shooter, x: S.shot.x, y: S.shot.y });
        toast('Shot spot: ' + (d.shooter === 'T' ? tname(S.shot.team) : plabel(d.shooter))); }
      S.shot = null; return render();
    }
    if (d.sides) { push({ op: 'sides' }); S.local.us_left = !st().us_left; return render(); }
    if (d.poss) { var q = d.poss.split('|'); addEv({ team: q[0], type: 'POSS', player_id: null, tag: q[1] }); toast(tname(q[0]) + ' ball - ' + POSS_LBL[q[1]]); return render(); }
    if (d.fb) { var cp = curPoss(); if (cp) { addEv({ team: cp.team, type: 'TRANS', player_id: null }); toast('Fast break: ' + tname(cp.team)); } return render(); }
    if (d.delposs) { var lp = curPoss(); if (lp) push({ op: 'del', cid: lp.cid }); return render(); }
    if (d.clock) { clockOp(d.clock); return render(); }
    if (d.timeout) { addEv({ team: d.timeout, type: 'TIMEOUT', player_id: null }); if (st().run) clockOp('stop'); toast(tname(d.timeout) + ' timeout'); return render(); }
    if (d.jump) { var first = !events().some(function (e) { return e.type === 'JUMP'; });
      addEv({ team: d.jump, type: 'JUMP', player_id: null, tag: first ? 'tip' : 'held' }); toast(tname(d.jump) + ' gets the ball'); return render(); }
    if (d.fix) { ['us', 'opp'].forEach(function (tt) { var want = Number($('cs-fix-' + tt).value), diff = want - score(tt);
        if (!isNaN(want) && diff) addEv({ team: tt, type: 'ADJ', player_id: null, points: diff }); }); toast('Score fixed'); return render(); }
    if (d.sub) { subTap.apply(null, d.sub.split('|')); return render(); }
    if (d.addbtn) { var inp = document.querySelector('[data-addp="' + d.addbtn + '"]'), m = (inp.value || '').trim().match(/^#?(\d+)\s*(.*)$/);
      if (!m) return toast('Type a number and an optional name, like 23 Smith', true);
      push({ op: 'addplayer', team: d.addbtn, number: m[1], name: m[2] || '' }); inp.value = ''; toast('Adding #' + m[1]); return; }
  }
  function subTap(t, id) {
    var lu = lineup(t), out = S.out[t];
    if (lu.indexOf(id) >= 0) { if (out === id) { lu = lu.filter(function (x) { return x !== id; }); S.out[t] = null; } else { S.out[t] = id; return; } }
    else if (out) { lu[lu.indexOf(out)] = id; S.out[t] = null; }
    else if (lu.length < 5) lu.push(id);
    else return toast('Tap the player coming out first, then the player going in.', true);
    S.local[t] = lu.join(','); push({ op: 'lineup', team: t, ids: lu });
  }
  function clockOp(k) {
    var s = st(), r = clockRem(); if (r == null) r = periodSecs(s.period || '1');
    if (k === 'toggle') k = s.run ? 'stop' : 'start';
    if (k === 'start') { S.local.run = true; S.local.rem = r; S.clockBase = { rem: r, at: performance.now(), run: true }; push({ op: 'clock', action: 'start' }); }
    else if (k === 'stop') { S.local.run = false; S.local.rem = r; S.clockBase = { rem: r, at: performance.now(), run: false }; push({ op: 'clock', action: 'stop' }); }
    else if (k === 'set') { var v = ($('cs-setclk').value || '').match(/^(\d+)(?::(\d{1,2}))?$/); if (!v) return toast('Type the time as m:ss', true);
      var secs = Number(v[1]) * (v[2] !== undefined ? 60 : 1) + Number(v[2] || 0); S.local.rem = secs;
      S.clockBase = { rem: secs, at: performance.now(), run: s.run }; push({ op: 'clock', action: 'set', value: secs }); }
    else { var dlt = Number(k); S.clockBase = { rem: Math.max(0, r + dlt), at: performance.now(), run: s.run }; push({ op: 'clock', action: 'adj', value: dlt }); }
  }
  function onChange(ev) {
    var t = ev.target;
    if (t.id === 'cs-game') { selectGame(t.value); }
    else if (t.id === 'cs-period') { S.local.period = t.value; S.local.run = false; S.local.rem = periodSecs(t.value);
      S.clockBase = { rem: S.local.rem, at: performance.now(), run: false }; push({ op: 'period', period: t.value }); }
    else if (t.id === 'cs-track') { S.local.track = t.checked; push({ op: 'track', on: t.checked }); }
  }
  function onInput(ev) { if (ev.target.id === 'cs-name') { S.keeper = ev.target.value; store.set('cs_keeper', S.keeper); } }
  function onKey(ev) {
    if (ev.target.id === 'cs-cmd') { if (ev.key === 'Enter') { ev.preventDefault(); S._focusCmd = true; runCmd(); } else if (ev.key === 'Escape') ev.target.value = ''; return; }
    var tag = ev.target.tagName;
    if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
    if ((ev.code === 'Space' || ev.key === ' ') && S.jobs.clock && S.snap && $('cs-live') && $('cs-live').offsetParent) {
      ev.preventDefault(); if (document.activeElement) document.activeElement.blur(); clockOp('toggle'); render();
    }
  }
  function onCourt(ev) {
    var svg = ev.target.closest('#cs-court'); if (!svg || !S.jobs.shots) return;
    var pt = svg.createSVGPoint(); pt.x = ev.clientX; pt.y = ev.clientY;
    var p = pt.matrixTransform(svg.getScreenCTM().inverse()), X = p.x / 10, Y = 50 - p.y / 10;
    if (X < 0 || X > 94 || Y < 0 || Y > 50) return;
    var h = toHalf(X, Y), team = (h.side === 'left') === usLeftNow() ? 'us' : 'opp';
    S.shot = { X: X, Y: Y, x: Math.round(h.x * 10) / 10, y: Math.round(h.y * 10) / 10, team: team };
    renderShots();
  }

  // ---------- server sync ----------
  function selectGame(gid) {
    S.gid = gid ? Number(gid) : null; S.snap = null; S.bt = null; S.btHist = []; S.local = {}; S.clockBase = null; S.sel = { us: 'T', opp: 'T' };
    store.set('cs_gid', S.gid);
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('live_game', S.gid || '', { priority: 'event' });
    render();
  }
  var flushTimer = null;
  function flushSoon() { clearTimeout(flushTimer); flushTimer = setTimeout(flush, 150); }
  function flush() {
    if (!S.online || !S.queue.length || !window.Shiny || !Shiny.setInputValue) return;
    if (S.inflight && Date.now() - S.inflightAt < 6000) return;
    var gid = S.queue[0].gid, batch = S.queue.filter(function (o) { return o.gid === gid; }).slice(0, 60);
    S.inflight = true; S.inflightAt = Date.now();
    Shiny.setInputValue('cs_batch', { gid: gid, ops: batch, keeper: S.keeper || 'Keeper', kid: S.kid }, { priority: 'event' });
  }
  function onSnap(m) {
    if (!m || String(m.gid) !== String(S.gid)) return;
    S.snap = m; S.offset = (m.server_now || Date.now()) - Date.now(); S.local = {};
    S.clockBase = { rem: m.state.rem, at: performance.now(), run: !!m.state.run };
    render();
  }
  function onAck(m) {
    var done = arr(m.oids).map(String);
    S.queue = S.queue.filter(function (o) { return done.indexOf(String(o.oid)) < 0; });
    S.inflight = false; save(); render(); if (S.queue.length) flushSoon();
  }
  function onGames(m) {
    S.games = rows(m.games); S.team = m.team || '';
    var keep = S.games.some(function (g) { return String(g.id) === String(S.gid); });
    if (!keep) { var saved = store.get('cs_gid', null); S.gid = S.games.some(function (g) { return String(g.id) === String(saved); }) ? saved : null; }
    if (S.gid && window.Shiny && (!S.snap || String(S.snap.gid) !== String(S.gid))) Shiny.setInputValue('live_game', S.gid, { priority: 'event' });
    render();
  }

  // ---------- start ----------
  function tick() {
    var el = $('cs-clock'), r = clockRem();
    if (el) el.textContent = r == null ? '--:--' : (r < 60 && st().run ? r.toFixed(1) : fmtClock(r));
    var bt_t = $('cs-bt-t'); if (bt_t && S.bt && S.bt.state === 'ball') bt_t.textContent = Math.max(0, (now() - S.bt.since) / 1000).toFixed(1);
    var pt = $('cs-pt'), cp = S.jobs.poss ? curPoss() : null;
    if (pt && cp) pt.textContent = Math.max(0, Math.round((now() - cp.ts) / 1000));
  }
  function init() {
    var root = $('cs-live'); if (!root || root._csInit) return; root._csInit = true;
    root.innerHTML = '<div id="cs-top" class="cs-top"></div><div id="cs-sb" class="cs-sb"></div><div id="cs-clockp" class="panel"></div>' +
      '<div id="cs-ball" class="panel"></div><div id="cs-actions" class="panel"></div><div id="cs-shots" class="panel"></div><div id="cs-poss" class="panel"></div><div id="cs-toast" class="cs-toast"></div>';
    root.addEventListener('click', onClick); root.addEventListener('change', onChange); root.addEventListener('input', onInput);
    root.addEventListener('pointerup', onCourt); document.addEventListener('keydown', onKey);
    S.gid = store.get('cs_gid', null);
    render(); setInterval(tick, 100); setInterval(flush, 2000);
  }
  function hookShiny() {
    Shiny.addCustomMessageHandler('cs_snap', onSnap);
    Shiny.addCustomMessageHandler('cs_ack', onAck);
    Shiny.addCustomMessageHandler('cs_games', onGames);
  }
  if (window.jQuery) {
    jQuery(document).on('shiny:connected', function () { S.online = true; Shiny.setInputValue('live_hello', Date.now(), { priority: 'event' });
      if (S.gid) Shiny.setInputValue('live_game', S.gid, { priority: 'event' }); S.inflight = false; render(); flushSoon(); });
    jQuery(document).on('shiny:disconnected', function () { S.online = false; S.inflight = false; render(); });
    jQuery(function () { init(); if (window.Shiny && Shiny.addCustomMessageHandler) hookShiny(); });
  }
  window.CourtSideLive = { state: S, events: events, parseCmd: parseCmd, bt: function () { return bt(); } };
})();
