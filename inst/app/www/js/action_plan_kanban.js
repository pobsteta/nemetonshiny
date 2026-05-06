// Kanban drag-and-drop init for the Plan d'actions tab.
//
// Exposed as `window.initKanbanSortable(boardId, dropInputId)`. The
// renderUI output emits an inline <script> at the end of every render
// that calls this function, which:
//
//   1. Tears down any previous Sortable instance bound to the column
//      bodies (renderUI rebuilds the DOM, so old instances would leak).
//   2. Creates one Sortable per column, all in the same `kanban` group
//      so cards can be moved across columns.
//   3. On drop into a different column, pushes a payload to the Shiny
//      input named `dropInputId` with `priority: "event"` so that the
//      same drop fires the observer reliably even when the previous
//      payload was identical.
//
// The server-side observer validates the transition and either
// persists the new status (which triggers a renderUI re-run via
// plan_rv()) or surfaces an error and bumps `kanban_render_token` to
// force a re-render that puts the card back where the data says it
// belongs.

(function() {
  if (typeof window === "undefined") return;

  window.initKanbanSortable = function(boardId, dropInputId) {
    if (typeof Sortable === "undefined") return;
    var board = document.getElementById(boardId);
    if (!board) return;

    var cols = board.querySelectorAll("[data-kanban-col]");
    cols.forEach(function(col) {
      if (col._sortable) {
        try { col._sortable.destroy(); } catch (e) {}
      }
      col._sortable = Sortable.create(col, {
        group: "kanban",
        animation: 150,
        ghostClass: "kanban-card-ghost",
        chosenClass: "kanban-card-chosen",
        onAdd: function(evt) {
          var item = evt.item;
          var actionId = item.getAttribute("data-action-id");
          var targetStatus = evt.to.getAttribute("data-kanban-col");
          var sourceStatus = evt.from.getAttribute("data-kanban-col");
          if (!actionId || !targetStatus) return;
          if (typeof Shiny === "undefined" || !Shiny.setInputValue) return;
          Shiny.setInputValue(dropInputId, {
            action_id: actionId,
            target_status: targetStatus,
            source_status: sourceStatus,
            nonce: Math.random()
          }, { priority: "event" });
        }
      });
    });
  };
})();
