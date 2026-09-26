/**
 * Test Data Explorer — bundle header, snapshot/layer/table tree, table viewer,
 * schema tab and overview charts.
 *
 * Adapted from Gilead-BioStats/open.gismo site/src/explorer.js @ 1072c620955a
 * (tree, search and selection wiring); see VENDORED.md. Data is embedded in the
 * widget payload instead of fetched, tree levels are snapshot → layer → table,
 * and charts are drawn with gsm.viz (global `gsmViz`).
 *
 *   gsmTestdataExplorer.render(el, x) → { resize(w, h) }
 */
(function () {
  'use strict';

  const ns = (window.gsmTestdataExplorer = window.gsmTestdataExplorer || {});
  const esc = (s) => ns.datatable.esc(s);
  const arr = (v) => (v === null || v === undefined) ? [] : (Array.isArray(v) ? v : [v]);

  /** SVG icons (from open.gismo explorer.js) */
  const dataIcon = '<svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><rect x="3" y="3" width="18" height="18" rx="2"/><line x1="3" y1="9" x2="21" y2="9"/><line x1="9" y1="3" x2="9" y2="21"/></svg>';
  const folderIcon = '<svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><path d="M22 19a2 2 0 01-2 2H4a2 2 0 01-2-2V5a2 2 0 012-2h5l2 3h9a2 2 0 012 2z"/></svg>';
  const chevron = '<svg viewBox="0 0 24 24" width="12" height="12" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round"><polyline points="9 18 15 12 9 6"/></svg>';

  const LAYERS = ['raw', 'mapped', 'analytics', 'reporting'];
  const layerOrder = (l) => { const i = LAYERS.indexOf(l); return i < 0 ? LAYERS.length : i; };
  const fmtInt = (n) => (n === null || n === undefined) ? '' : Number(n).toLocaleString();

  // ── Data shaping ───────────────────────────────────────────────────────────

  /** rows of x.tables → { [snapshot]: { [layer]: [table row, …] } } */
  function groupTables(tables) {
    const tree = {};
    for (const t of tables) {
      (tree[t.snapshot] = tree[t.snapshot] || {});
      (tree[t.snapshot][t.layer] = tree[t.snapshot][t.layer] || []).push(t);
    }
    return tree;
  }

  // ── Header ─────────────────────────────────────────────────────────────────

  function buildHeader(m, x) {
    const el = document.createElement('div');
    el.className = 'gte-header';
    const snaps = arr(m.snapshots);
    const pills = [
      ['Bundle', 'v' + esc(m.bundle_version)],
      ['Study', esc(m.study_id)],
      ['Seed', esc(m.seed)],
      ['Participants', fmtInt(m.participants)],
      ['Sites', fmtInt(m.sites)],
      ['Snapshots', snaps.length ? `${snaps.length} (${esc(snaps[0])} → ${esc(snaps[snaps.length - 1])})` : '0'],
      ['Tables', fmtInt(arr(x.tables).length)],
    ];
    const versions = m.package_versions || {};
    const vers = Object.keys(versions).filter(k => versions[k] !== null)
      .map(k => `<span class="gte-ver"><b>${esc(k)}</b> ${esc(versions[k])}</span>`).join('');
    el.innerHTML =
      `<div class="gte-title">${dataIcon} <span>Test Data Explorer</span>` +
      `<span class="gte-built">built ${esc(m.built_at || '')} by ${esc(m.built_by || '')}` +
      (m.workflow_run ? ` · <a href="${esc(m.workflow_run)}" target="_blank" rel="noopener">workflow run</a>` : '') +
      `</span></div>` +
      `<div class="gte-pills">${pills.map(([k, v]) => `<span class="gte-pill"><span class="gte-pill-k">${k}</span><span class="gte-pill-v">${v}</span></span>`).join('')}</div>` +
      (vers ? `<div class="gte-versions">${vers}</div>` : '');
    return el;
  }

  // ── Sidebar tree (adapted from open.gismo buildSidebarTree) ─────────────────

  function buildSidebarTree(tree, latest) {
    let h = '';
    const snapshots = Object.keys(tree).sort();
    for (const snap of snapshots) {
      const open = snap === latest;
      h += `<div class="explorer-phase">`;
      h += `<div class="explorer-phase-header ${open ? '' : 'collapsed'}" data-phase="${esc(snap)}">${chevron} ${folderIcon} <span>${esc(snap)}</span>${open ? '<span class="gte-tag">latest</span>' : ''}</div>`;
      h += `<div class="explorer-phase-children" style="${open ? '' : 'display:none'}">`;
      const layers = Object.keys(tree[snap]).sort((a, b) => layerOrder(a) - layerOrder(b));
      for (const layer of layers) {
        const items = tree[snap][layer];
        const openLayer = open && layer === 'raw';
        h += `<div class="explorer-wf">`;
        h += `<div class="explorer-wf-header ${openLayer ? '' : 'collapsed'}" data-wf="${esc(layer)}">${chevron} ${folderIcon} <span>${esc(layer)}</span><span class="gte-count">${items.length}</span></div>`;
        h += `<div class="explorer-wf-children" style="${openLayer ? '' : 'display:none'}">`;
        for (const t of items) {
          const key = `${t.snapshot}/${t.layer}/${t.table}`;
          h += `<div class="explorer-item" data-key="${esc(key)}" data-search="${esc((t.table + ' ' + t.layer + ' ' + t.snapshot).toLowerCase())}" title="${esc(t.table)}: ${fmtInt(t.rows)} rows × ${fmtInt(t.cols)} cols">`;
          h += `${dataIcon} <span class="explorer-item-name">${esc(t.table)}</span>`;
          h += `<span class="gte-dims">${fmtInt(t.rows)}×${fmtInt(t.cols)}</span>`;
          h += `</div>`;
        }
        h += `</div></div>`;
      }
      h += `</div></div>`;
    }
    return h;
  }

  // ── Viewer panels ──────────────────────────────────────────────────────────

  function buildSchemaTable(columns) {
    const wrap = document.createElement('div');
    wrap.className = 'gte-schema';
    if (!columns.length) { wrap.innerHTML = '<div class="dt-empty">No column summary</div>'; return wrap; }
    let h = '<table class="dt-table gte-schema-table"><thead><tr><th>Column</th><th>Type</th><th class="dt-num">Missing %</th><th class="dt-num">Distinct</th><th>Example</th></tr></thead><tbody>';
    columns.forEach((c, i) => {
      h += `<tr class="${i % 2 ? 'dt-alt' : ''}"><td><code>${esc(c.column)}</code></td><td>${esc(c.type)}</td>` +
        `<td class="dt-num">${c.pct_missing === null ? '' : esc(c.pct_missing)}</td>` +
        `<td class="dt-num">${fmtInt(c.n_distinct)}</td><td class="gte-example">${esc(c.example === null ? '' : c.example)}</td></tr>`;
    });
    h += '</tbody></table>';
    wrap.innerHTML = h;
    return wrap;
  }

  function showTable(viewer, x, key) {
    const [snapshot, layer, table] = key.split('/');
    const d = x.data[key];
    const cols = arr(x.columns).filter(c => c.snapshot === snapshot && c.layer === layer && c.table === table);
    viewer.innerHTML = '';

    const header = document.createElement('div');
    header.className = 'explorer-viewer-header';
    header.innerHTML =
      `<span class="explorer-viewer-title">${dataIcon} ${esc(table)}</span>` +
      `<span class="explorer-viewer-snap">${esc(layer)} · ${esc(snapshot)} · ${fmtInt(d ? d.total_rows : 0)} rows × ${fmtInt(d ? arr(d.columns).length : 0)} cols</span>`;
    viewer.appendChild(header);

    const tabs = document.createElement('div');
    tabs.className = 'gte-tabs';
    tabs.innerHTML = `<button class="gte-tab active" data-tab="data">Data</button><button class="gte-tab" data-tab="schema">Schema</button>`;
    viewer.appendChild(tabs);

    const body = document.createElement('div');
    body.className = 'gte-tab-body';
    viewer.appendChild(body);

    const panes = {};
    const renderData = () => {
      const pane = document.createElement('div');
      pane.className = 'gte-pane';
      if (d && d.truncated) {
        const note = document.createElement('div');
        note.className = 'gte-note';
        note.textContent = `Showing the first ${fmtInt(arr(d.rows).length)} of ${fmtInt(d.total_rows)} rows. Summary statistics cover the full table.`;
        pane.appendChild(note);
      }
      const tbl = ns.datatable.buildTableFromRows(d ? arr(d.columns) : [], d ? arr(d.rows) : []);
      tbl.classList.add('explorer-table-wrap');
      pane.appendChild(tbl);
      return pane;
    };
    const renderSchema = () => {
      const pane = document.createElement('div');
      pane.className = 'gte-pane';
      pane.appendChild(buildSchemaTable(cols));
      return pane;
    };
    const activate = (name) => {
      tabs.querySelectorAll('.gte-tab').forEach(b => b.classList.toggle('active', b.dataset.tab === name));
      body.innerHTML = '';
      if (!panes[name]) panes[name] = name === 'data' ? renderData() : renderSchema();
      body.appendChild(panes[name]);
    };
    tabs.addEventListener('click', (e) => {
      const b = e.target.closest('.gte-tab');
      if (b) activate(b.dataset.tab);
    });
    activate('data');
  }

  // ── Overview (charts) ──────────────────────────────────────────────────────

  function fallbackBars(el, data, xKey, yKey) {
    // Minimal HTML bar list used when gsm.viz is unavailable or throws.
    const max = Math.max(1, ...data.map(d => Number(d[yKey]) || 0));
    el.innerHTML = '<div class="gte-fallback">' + data.slice(0, 40).map(d =>
      `<div class="gte-fb-row"><span class="gte-fb-label">${esc(d[xKey])}</span>` +
      `<span class="gte-fb-bar"><span style="width:${(100 * (Number(d[yKey]) || 0) / max).toFixed(1)}%"></span></span>` +
      `<span class="gte-fb-val">${fmtInt(d[yKey])}</span></div>`).join('') + '</div>';
  }

  // The gsm.viz bundle is built with an esbuild global name, so its API lives on
  // `gsmViz.default` (gsm.kri's widgets call it the same way).
  function viz() {
    const g = window.gsmViz;
    if (!g) return null;
    return (g.default && typeof g.default.bars === 'function') ? g.default : g;
  }

  function drawBars(el, data, spec, xKey, yKey) {
    if (!data.length) { el.innerHTML = '<div class="dt-empty">No data</div>'; return; }
    try {
      const v = viz();
      if (v && typeof v.bars === 'function') {
        v.bars(el, data, Object.assign({ stat: 'identity', interactive: false }, spec));
        return;
      }
    } catch (err) {
      console.warn('gsm.viz bars failed, using fallback:', err); // eslint-disable-line no-console
    }
    fallbackBars(el, data, xKey, yKey);
  }

  function chartCard(title, subtitle) {
    const card = document.createElement('div');
    card.className = 'gte-card';
    card.innerHTML = `<div class="gte-card-title">${esc(title)}</div>` + (subtitle ? `<div class="gte-card-sub">${esc(subtitle)}</div>` : '');
    const body = document.createElement('div');
    body.className = 'gte-card-body';
    card.appendChild(body);
    return { card, body };
  }

  function showOverview(viewer, x) {
    viewer.innerHTML = '';
    const charts = x.charts || {};
    const latest = x.latest;

    const header = document.createElement('div');
    header.className = 'explorer-viewer-header';
    header.innerHTML = `<span class="explorer-viewer-title">${folderIcon} Overview</span><span class="explorer-viewer-snap">charts use the latest snapshot (${esc(latest)})</span>`;
    viewer.appendChild(header);

    const grid = document.createElement('div');
    grid.className = 'gte-grid';
    viewer.appendChild(grid);

    // Rows per table across snapshots, one layer at a time.
    const rbs = arr(charts.rows_by_snapshot);
    const layersPresent = LAYERS.filter(l => rbs.some(r => r.layer === l));
    const rowsCard = chartCard('Rows per table across snapshots', 'how each table grows from snapshot to snapshot');
    rowsCard.card.classList.add('gte-card-wide');
    const sel = document.createElement('select');
    sel.className = 'gte-select';
    layersPresent.forEach(l => { const o = document.createElement('option'); o.value = l; o.textContent = l; sel.appendChild(o); });
    rowsCard.card.insertBefore(sel, rowsCard.body);
    const drawRows = () => {
      const layer = sel.value;
      const data = rbs.filter(r => r.layer === layer).map(r => ({ table: r.table, rows: Number(r.rows), snapshot: r.snapshot }));
      rowsCard.body.style.height = '360px';
      rowsCard.body.innerHTML = '';
      drawBars(rowsCard.body, data, {
        mapping: { x: 'table', y: 'rows', fill: 'snapshot' },
        position: 'dodge',
        labels: { x: '', y: 'rows', fill: 'snapshot' },
      }, 'table', 'rows');
    };
    sel.addEventListener('change', drawRows);
    grid.appendChild(rowsCard.card);

    const sitesCard = chartCard('Subjects per site', `Raw_SUBJ, ${esc(latest)}`);
    grid.appendChild(sitesCard.card);
    const aeCard = chartCard('Adverse events per subject', `Raw_AE joined to Raw_SUBJ, ${esc(latest)}`);
    grid.appendChild(aeCard.card);
    const enrolCard = chartCard('Enrolment over time', `subjects enrolled per month, ${esc(latest)}`);
    grid.appendChild(enrolCard.card);

    // Draw after the cards are in the DOM so canvases get a size.
    requestAnimationFrame(() => {
      if (layersPresent.length) drawRows();
      drawBars(sitesCard.body, arr(charts.subjects_per_site).map(d => ({ site: d.site, subjects: Number(d.subjects) })),
        { mapping: { x: 'site', y: 'subjects' }, labels: { x: 'site', y: 'subjects' } }, 'site', 'subjects');
      drawBars(aeCard.body, arr(charts.aes_per_subject).map(d => ({ aes: d.aes, subjects: Number(d.subjects) })),
        { mapping: { x: 'aes', y: 'subjects' }, labels: { x: 'AEs per subject', y: 'subjects' } }, 'aes', 'subjects');
      drawBars(enrolCard.body, arr(charts.enrollment_over_time).map(d => ({ month: d.month, enrolled: Number(d.enrolled) })),
        { mapping: { x: 'month', y: 'enrolled' }, labels: { x: 'month', y: 'enrolled' } }, 'month', 'enrolled');
    });
  }

  // ── Explorer (adapted from open.gismo buildExplorer) ───────────────────────

  function render(rootEl, x) {
    const tables = arr(x.tables);
    const tree = groupTables(tables);

    const root = document.createElement('div');
    root.className = 'gte-root';
    rootEl.appendChild(root);
    root.appendChild(buildHeader(x.manifest || {}, x));

    const el = document.createElement('div');
    el.className = 'explorer-layout';
    root.appendChild(el);

    // Sidebar
    const sidebar = document.createElement('div');
    sidebar.className = 'explorer-sidebar';
    sidebar.innerHTML =
      `<div class="explorer-search"><input type="text" placeholder="Search tables…" aria-label="Search tables" class="explorer-search-input"></div>` +
      `<div class="explorer-overview-link explorer-item selected" data-key="__overview__">${folderIcon} <span class="explorer-item-name">Overview</span></div>` +
      `<div class="explorer-tree">${tables.length ? buildSidebarTree(tree, x.latest) : '<div class="explorer-empty">No tables in bundle</div>'}</div>`;
    el.appendChild(sidebar);

    // Main viewer
    const viewer = document.createElement('div');
    viewer.className = 'explorer-viewer';
    el.appendChild(viewer);

    // Tree expand/collapse
    sidebar.querySelectorAll('.explorer-phase-header, .explorer-wf-header').forEach(hdr => {
      hdr.addEventListener('click', () => {
        const children = hdr.nextElementSibling;
        const expanded = children.style.display !== 'none';
        children.style.display = expanded ? 'none' : '';
        hdr.classList.toggle('collapsed', expanded);
      });
    });

    // Search
    const searchInput = sidebar.querySelector('.explorer-search-input');
    searchInput.addEventListener('input', () => {
      const q = searchInput.value.toLowerCase();
      sidebar.querySelectorAll('.explorer-tree .explorer-item').forEach(item => {
        const match = !q || item.dataset.search.includes(q);
        item.style.display = match ? '' : 'none';
      });
      sidebar.querySelectorAll('.explorer-wf').forEach(wf => {
        const hasVisible = wf.querySelector('.explorer-item:not([style*="display: none"])');
        wf.style.display = hasVisible ? '' : 'none';
        const children = wf.querySelector('.explorer-wf-children');
        const hdr = wf.querySelector('.explorer-wf-header');
        if (q && hasVisible) { children.style.display = ''; hdr.classList.remove('collapsed'); }
      });
      sidebar.querySelectorAll('.explorer-phase').forEach(phase => {
        const hasVisible = phase.querySelector('.explorer-wf:not([style*="display: none"])');
        phase.style.display = hasVisible ? '' : 'none';
        const children = phase.querySelector('.explorer-phase-children');
        const hdr = phase.querySelector('.explorer-phase-header');
        if (q && hasVisible) { children.style.display = ''; hdr.classList.remove('collapsed'); }
      });
    });

    // Selection
    sidebar.addEventListener('click', (e) => {
      const item = e.target.closest('.explorer-item');
      if (!item) return;
      sidebar.querySelectorAll('.explorer-item').forEach(i => i.classList.remove('selected'));
      item.classList.add('selected');
      const key = item.dataset.key;
      if (key === '__overview__') showOverview(viewer, x);
      else showTable(viewer, x, key);
    });

    showOverview(viewer, x);

    return {
      resize: function () { /* layout is flex-based; charts are redrawn on next selection */ },
    };
  }

  ns.render = render;
  ns.groupTables = groupTables;
})();
