/**
 * Enhanced data table renderer with sort, search, and pagination.
 *
 * VENDORED from Gilead-BioStats/open.gismo site/src/datatable.js @ 1072c620955a
 * (see VENDORED.md). Changes: no ES modules (IIFE publishing
 * window.gsmTestdataExplorer.datatable), `esc()` inlined from utils.js, and
 * the renderer split into buildTable({headers, rows}) so JSON rows can be
 * rendered directly via buildTableFromRows(headers, rows).
 *
 *   buildEnhancedTable(csvText)        → HTMLElement (original entry point)
 *   buildTableFromRows(headers, rows)  → HTMLElement (rows: arrays of cells)
 *
 *   - Sortable columns (click header to toggle asc/desc/none)
 *   - Global search filter
 *   - Pagination with configurable page size
 *   - Column type detection (numeric, text)
 *   - Row count and column summary stats
 */
(function () {
  'use strict';

  function esc(s) {
    if (s === null || s === undefined || s === '') return '';
    const d = document.createElement('div');
    d.textContent = String(s);
    return d.innerHTML;
  }

  const PAGE_SIZES = [10, 25, 50, 100];
  const DEFAULT_PAGE_SIZE = 25;

  // ── CSV parsing ────────────────────────────────────────────────────────────

  function parseCsv(text) {
    const trimmed = (text || '').trim();
    if (!trimmed) return { headers: [], rows: [] };
    const lines = trimmed.split('\n');
    const parse = (line) => {
      const fields = [];
      let cur = '', inQ = false;
      for (let i = 0; i < line.length; i++) {
        const ch = line[i];
        if (inQ) {
          if (ch === '"' && line[i + 1] === '"') { cur += '"'; i++; }
          else if (ch === '"') inQ = false;
          else cur += ch;
        } else {
          if (ch === '"') inQ = true;
          else if (ch === ',') { fields.push(cur.trim()); cur = ''; }
          else cur += ch;
        }
      }
      fields.push(cur.trim());
      return fields;
    };
    return { headers: parse(lines[0]), rows: lines.slice(1).filter(l => l.trim()).map(parse) };
  }

  // ── Column analysis ────────────────────────────────────────────────────────

  function detectType(values) {
    const nonEmpty = values.filter(v => v !== '' && v !== 'NA' && v !== 'null');
    if (!nonEmpty.length) return 'text';
    const numCount = nonEmpty.filter(v => !isNaN(Number(v))).length;
    if (numCount / nonEmpty.length > 0.8) return 'numeric';
    return 'text';
  }

  function numericStats(values) {
    const nums = values.map(Number).filter(n => !isNaN(n));
    if (!nums.length) return null;
    nums.sort((a, b) => a - b);
    const n = nums.length;
    const sum = nums.reduce((a, b) => a + b, 0);
    const mean = sum / n;
    const min = nums[0], max = nums[n - 1];
    const median = n % 2 ? nums[(n - 1) / 2] : (nums[n / 2 - 1] + nums[n / 2]) / 2;
    const variance = nums.reduce((s, v) => s + (v - mean) ** 2, 0) / n;
    const sd = Math.sqrt(variance);
    return { n, mean, median, min, max, sd };
  }

  // ── Sort helpers ───────────────────────────────────────────────────────────

  const SORT_NONE = 0, SORT_ASC = 1, SORT_DESC = 2;
  const SORT_ICONS = ['↕', '↑', '↓'];

  function compareValues(a, b, type) {
    if (type === 'numeric') {
      const na = Number(a), nb = Number(b);
      if (isNaN(na) && isNaN(nb)) return 0;
      if (isNaN(na)) return 1;
      if (isNaN(nb)) return -1;
      return na - nb;
    }
    return String(a).localeCompare(String(b));
  }

  // ── Stats tooltip ──────────────────────────────────────────────────────────

  function formatNum(n) {
    if (Number.isInteger(n)) return String(n);
    return n.toFixed(3);
  }

  function buildStatsTooltip(stats) {
    if (!stats) return '';
    return `n=${stats.n} | min=${formatNum(stats.min)} | mean=${formatNum(stats.mean)} | median=${formatNum(stats.median)} | max=${formatNum(stats.max)} | sd=${formatNum(stats.sd)}`;
  }

  // ── Main render ────────────────────────────────────────────────────────────

  function buildTable({ headers, rows: allRows }) {
    if (!headers.length) {
      const empty = document.createElement('div');
      empty.className = 'dt-empty';
      empty.textContent = 'No data';
      return empty;
    }

    // Analyze columns
    const colTypes = headers.map((_, ci) => detectType(allRows.map(r => r[ci] || '')));
    const colStats = headers.map((_, ci) =>
      colTypes[ci] === 'numeric' ? numericStats(allRows.map(r => r[ci] || '')) : null
    );
    // State
    let filteredRows = allRows;
    let sortCol = -1, sortDir = SORT_NONE;
    let page = 0, pageSize = DEFAULT_PAGE_SIZE;
    let searchTerm = '';

    const root = document.createElement('div');
    root.className = 'dt-root';

    // ────── Toolbar ──────
    const toolbar = document.createElement('div');
    toolbar.className = 'dt-toolbar';
    toolbar.innerHTML = `
      <div class="dt-toolbar-left">
        <span class="dt-row-count"></span>
      </div>
      <div class="dt-toolbar-right">
        <input type="text" class="dt-search" placeholder="Search…" aria-label="Search table">
        <select class="dt-page-size" aria-label="Rows per page">
          ${PAGE_SIZES.map(s => `<option value="${s}"${s === DEFAULT_PAGE_SIZE ? ' selected' : ''}>${s} rows</option>`).join('')}
        </select>
      </div>`;
    root.appendChild(toolbar);

    // ────── Table ──────
    const tableWrap = document.createElement('div');
    tableWrap.className = 'dt-table-wrap';
    const table = document.createElement('table');
    table.className = 'dt-table';
    tableWrap.appendChild(table);
    root.appendChild(tableWrap);

    // ────── Pagination ──────
    const pager = document.createElement('div');
    pager.className = 'dt-pager';
    root.appendChild(pager);

    // ────── Render logic ──────
    function applyFilter() {
      const q = searchTerm.toLowerCase();
      filteredRows = q
        ? allRows.filter(r => r.some(cell => cell.toLowerCase().includes(q)))
        : allRows;
    }

    function applySort() {
      if (sortCol < 0 || sortDir === SORT_NONE) return;
      const type = colTypes[sortCol];
      const dir = sortDir === SORT_ASC ? 1 : -1;
      filteredRows = [...filteredRows].sort((a, b) =>
        dir * compareValues(a[sortCol] || '', b[sortCol] || '', type)
      );
    }

    function getPageRows() {
      const start = page * pageSize;
      return filteredRows.slice(start, start + pageSize);
    }

    function render() {
      applyFilter();
      applySort();
      const pageRows = getPageRows();
      const totalPages = Math.max(1, Math.ceil(filteredRows.length / pageSize));
      if (page >= totalPages) page = totalPages - 1;

      // Row count
      const countEl = toolbar.querySelector('.dt-row-count');
      countEl.textContent = searchTerm
        ? `${filteredRows.length} of ${allRows.length} rows × ${headers.length} cols`
        : `${allRows.length} rows × ${headers.length} cols`;

      // Table
      let html = '<thead>';
      html += '<tr>';
      headers.forEach((h, ci) => {
        const icon = ci === sortCol ? SORT_ICONS[sortDir] : SORT_ICONS[SORT_NONE];
        const typeTag = colTypes[ci] === 'numeric' ? '<span class="dt-type-tag">#</span>' : '<span class="dt-type-tag">Aa</span>';
        html += `<th data-col="${ci}" class="dt-sortable" title="${colStats[ci] ? esc(buildStatsTooltip(colStats[ci])) : ''}">`;
        html += `<div class="dt-th-content"><span class="dt-th-name">${esc(h)}</span>${typeTag}<span class="dt-sort-icon">${icon}</span></div>`;
        html += '</th>';
      });
      html += '</tr></thead><tbody>';

      if (pageRows.length === 0) {
        html += `<tr><td colspan="${headers.length}" class="dt-no-results">No matching rows</td></tr>`;
      } else {
        pageRows.forEach((row, ri) => {
          html += `<tr class="${ri % 2 ? 'dt-alt' : ''}">`;
          row.forEach((cell, ci) => {
            const cls = colTypes[ci] === 'numeric' ? ' class="dt-num"' : '';
            html += `<td${cls}>${esc(cell)}</td>`;
          });
          html += '</tr>';
        });
      }
      html += '</tbody>';
      table.innerHTML = html;

      // Pagination
      const start = page * pageSize + 1;
      const end = Math.min((page + 1) * pageSize, filteredRows.length);
      let pgHtml = `<span class="dt-page-info">${filteredRows.length ? start : 0}–${end} of ${filteredRows.length}</span>`;
      pgHtml += `<div class="dt-page-btns">`;
      pgHtml += `<button class="dt-pg-btn" data-pg="first" ${page === 0 ? 'disabled' : ''}>«</button>`;
      pgHtml += `<button class="dt-pg-btn" data-pg="prev" ${page === 0 ? 'disabled' : ''}>‹</button>`;
      pgHtml += `<span class="dt-page-num">Page ${page + 1} of ${totalPages}</span>`;
      pgHtml += `<button class="dt-pg-btn" data-pg="next" ${page >= totalPages - 1 ? 'disabled' : ''}>›</button>`;
      pgHtml += `<button class="dt-pg-btn" data-pg="last" ${page >= totalPages - 1 ? 'disabled' : ''}>»</button>`;
      pgHtml += `</div>`;
      pager.innerHTML = pgHtml;
    }

    // ────── Events ──────
    toolbar.querySelector('.dt-search').addEventListener('input', (e) => {
      searchTerm = e.target.value;
      page = 0;
      render();
    });

    toolbar.querySelector('.dt-page-size').addEventListener('change', (e) => {
      pageSize = Number(e.target.value);
      page = 0;
      render();
    });

    table.addEventListener('click', (e) => {
      const th = e.target.closest('th.dt-sortable');
      if (!th) return;
      const ci = Number(th.dataset.col);
      if (sortCol === ci) {
        sortDir = (sortDir + 1) % 3;
      } else {
        sortCol = ci;
        sortDir = SORT_ASC;
      }
      page = 0;
      render();
    });

    pager.addEventListener('click', (e) => {
      const btn = e.target.closest('.dt-pg-btn');
      if (!btn || btn.disabled) return;
      const totalPages = Math.max(1, Math.ceil(filteredRows.length / pageSize));
      switch (btn.dataset.pg) {
        case 'first': page = 0; break;
        case 'prev': page = Math.max(0, page - 1); break;
        case 'next': page = Math.min(totalPages - 1, page + 1); break;
        case 'last': page = totalPages - 1; break;
      }
      render();
    });

    render();
    return root;
  }

  function buildEnhancedTable(csvText) {
    return buildTable(parseCsv(csvText));
  }

  // rows: arrays of raw cell values (numbers, strings, null); stringified here so
  // the original filter/sort/type logic applies unchanged.
  function buildTableFromRows(headers, rows) {
    const cells = (rows || []).map(r => r.map(v => (v === null || v === undefined) ? '' : String(v)));
    return buildTable({ headers: headers || [], rows: cells });
  }

  window.gsmTestdataExplorer = window.gsmTestdataExplorer || {};
  window.gsmTestdataExplorer.datatable = { parseCsv, buildEnhancedTable, buildTableFromRows, esc };
})();
