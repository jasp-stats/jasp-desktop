// md_table_enhance.js — post-process markdown tables to match JASP native table structure.
// Adds column-type classes (text/number), detects row groups via first-column repetition,
// merges repeated values (combined), and marks group boundaries (new-group / last-in-group).
//
// Called from: md_text.js (results window — post marked.parse)
//               chat-bridge.js (chat window — MutationObserver on deep-chat shadow DOM)

(function () {
  "use strict";

  function getCellText(cell) {
    if (!cell) return "";
    return (cell.textContent || "").trim();
  }

  function detectColumnType(tbody, colIndex) {
    var rows = tbody.querySelectorAll("tr");
    var allNumeric = true;
    var anyContent = false;

    for (var r = 0; r < rows.length; r++) {
      var cell = rows[r].querySelectorAll("td, th")[colIndex];
      if (!cell) continue;
      var text = getCellText(cell);
      if (text === "" || text === "\u00A0") continue;
      anyContent = true;
      // Normalize Unicode minus signs to hyphen-minus for number detection
      var cleaned = text.replace(/[\u2012\u2013\u2014\u2212]/g, "-");
      if (isNaN(parseFloat(cleaned.replace(/,/g, "")))) {
        allNumeric = false;
        break;
      }
    }

    return anyContent && allNumeric ? "number" : "text";
  }

  function enhanceTable(table) {
    // Skip tables that JASP already rendered (they have jasp-no-select class)
    if (
      table.classList.contains("jasp-no-select") ||
      table.classList.contains("jasp-table-enhanced")
    )
      return;

    table.classList.add("jasp-table-enhanced");

    var tbody = table.querySelector("tbody");
    if (!tbody) return;

    var rows = tbody.querySelectorAll("tr");
    if (rows.length === 0) return;

    // --- 1. Column type detection ---
    var firstRow = rows[0];
    var sampleCells = firstRow.querySelectorAll("td, th");
    var colCount = sampleCells.length;
    var colTypes = [];

    for (var c = 0; c < colCount; c++) {
      colTypes[c] = detectColumnType(tbody, c);
    }

    // --- 2. Apply type classes to header cells ---
    // (skip — headers are always centered via thead th CSS)

    // --- 3. Apply type classes to body cells ---
    for (var r = 0; r < rows.length; r++) {
      var rowCells = rows[r].querySelectorAll("td, th");
      for (var c = 0; c < rowCells.length && c < colCount; c++) {
        rowCells[c].classList.add(colTypes[c]);
      }
    }

    // --- 4. Group detection & combine (only if first column is text) ---
    if (colTypes[0] === "text" && rows.length >= 2) {
      var prevVal = getCellText(rows[0].querySelector("td, th"));

      for (var r = 1; r < rows.length; r++) {
        var curCell = rows[r].querySelector("td, th");
        var curVal = getCellText(curCell);

        if (curVal !== prevVal || prevVal === "") {
          // New group
          rows[r].classList.add("new-group");
        } else if (curCell) {
          // Same group — merge repeated value
          curCell.innerHTML = "&nbsp;";
          curCell.classList.add("combined");
        }
        prevVal = curVal;
      }

      // Mark last-in-group rows (row before each new-group, plus last row)
      for (var r = 0; r < rows.length - 1; r++) {
        if (rows[r + 1].classList.contains("new-group")) {
          rows[r].classList.add("last-in-group");
        }
      }
      if (!rows[0].classList.contains("new-group")) {
        rows[0].classList.add("new-group");
      }
      rows[rows.length - 1].classList.add("last-in-group");
    }
  }

  window.enhanceMarkdownTables = function (container) {
    if (!container) return;
    var tables = container.querySelectorAll
      ? container.querySelectorAll("table")
      : [];
    for (var t = 0; t < tables.length; t++) {
      enhanceTable(tables[t]);
    }
  };
})();
