// Kanban drag-and-drop + double-click-to-edit init for the
// Plan d'actions tab.
//
// Exposed as `window.initKanbanSortable(boardId, dropInputId,
// editInputId)`. The renderUI output emits an inline <script> at the
// end of every render that calls this function, which:
//
//   1. Tears down any previous Sortable instance bound to the column
//      bodies (renderUI rebuilds the DOM, so old instances would leak).
//   2. Creates one Sortable per column, all in the same `kanban` group
//      so cards can be moved across columns. The Kanban allows free
//      movement: any source → any target.
//   3. On drop into a different column, pushes a payload to the Shiny
//      input named `dropInputId` with `priority: "event"` so that the
//      same drop fires the observer reliably even when the previous
//      payload was identical.
//   4. Attaches a single delegated `dblclick` listener at the board
//      level. Double-clicking any `[data-action-id]` card pushes the
//      action_id to `editInputId`, which the server uses to open an
//      edit modal pre-filled with the action's current values
//      (commentaire is the primary use-case).

(function() {
  if (typeof window === "undefined") return;

  window.initKanbanSortable = function(boardId, dropInputId, editInputId) {
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

    // Double-click → edit. We bind a single delegated listener at the
    // board level; renderUI rebuilds the board DOM, so any previous
    // listener attached to the same node is replaced when the new
    // node appears. We still guard against re-binding the same
    // function if the node persisted across renders.
    if (editInputId) {
      if (board._kanbanDblclickHandler) {
        board.removeEventListener("dblclick", board._kanbanDblclickHandler);
      }
      board._kanbanDblclickHandler = function(evt) {
        var card = evt.target.closest("[data-action-id]");
        if (!card || !board.contains(card)) return;
        var actionId = card.getAttribute("data-action-id");
        if (!actionId) return;
        if (typeof Shiny === "undefined" || !Shiny.setInputValue) return;
        Shiny.setInputValue(editInputId, {
          action_id: actionId,
          nonce: Math.random()
        }, { priority: "event" });
      };
      board.addEventListener("dblclick", board._kanbanDblclickHandler);
    }
  };
})();
