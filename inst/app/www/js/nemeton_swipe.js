/*
 * nemeton_swipe.js — comparaison « swipe » de deux rasters sur une carte Leaflet.
 *
 * Sans plugin : deux rasters vivent dans deux panes dédiés (gauche / droite) ; un
 * volet vertical draggable clippe les panes de part et d'autre. Même mécanique de
 * clip que leaflet-side-by-side (rect() en coordonnées LayerPoint), mais appliquée
 * à des image-overlays (`addRasterImage`), ce que le plugin R ne sait pas faire.
 *
 * Piloté par Shiny :
 *   session$sendCustomMessage("nemetonSwipeOn",  list(id=<mapOutputId>, left=<pane>, right=<pane>))
 *   session$sendCustomMessage("nemetonSwipeOff", list(id=<mapOutputId>))
 * La carte est retrouvée via HTMLWidgets.find("#id").getMap() (enregistrée par
 * htmlwidgets::onRender côté R).
 */
(function () {
  var state = {}; // mapOutputId -> swipe state

  function findMap(id) {
    try {
      var w = window.HTMLWidgets && HTMLWidgets.find("#" + id);
      if (w && typeof w.getMap === "function") return w.getMap();
    } catch (e) {}
    // repli : la carte enregistrée par onRender
    return (window.nemetonMaps && window.nemetonMaps[id]) || null;
  }

  function updateClip(s) {
    if (!s || !s.map) return;
    var map = s.map;
    var size = map.getSize();
    var nw = map.containerPointToLayerPoint([0, 0]);
    var se = map.containerPointToLayerPoint([size.x, size.y]);
    var clipX = map.containerPointToLayerPoint([s.pos, 0]).x;
    var lp = map.getPane(s.left);
    var rp = map.getPane(s.right);
    if (lp) lp.style.clip = "rect(" + nw.y + "px," + clipX + "px," + se.y + "px," + nw.x + "px)";
    if (rp) rp.style.clip = "rect(" + nw.y + "px," + se.x + "px," + se.y + "px," + clipX + "px)";
  }

  function place(s) {
    if (s.divider) s.divider.style.left = s.pos + "px";
    updateClip(s);
  }

  function swipeOn(msg) {
    swipeOff({ id: msg.id }); // idempotent
    var map = findMap(msg.id);
    if (!map) return;
    var container = map.getContainer();

    var divider = document.createElement("div");
    divider.className = "nemeton-swipe-divider";
    var handle = document.createElement("div");
    handle.className = "nemeton-swipe-handle";
    divider.appendChild(handle);
    container.appendChild(divider);

    var s = {
      map: map, left: msg.left, right: msg.right, divider: divider,
      pos: Math.round(map.getSize().x / 2)
    };
    state[msg.id] = s;

    var dragging = false;
    function down(e) { dragging = true; map.dragging.disable(); if (e.cancelable) e.preventDefault(); }
    function move(e) {
      if (!dragging) return;
      var rect = container.getBoundingClientRect();
      var cx = (e.touches && e.touches[0] ? e.touches[0].clientX : e.clientX) - rect.left;
      s.pos = Math.max(0, Math.min(rect.width, cx));
      place(s);
    }
    function up() { if (dragging) { dragging = false; map.dragging.enable(); } }

    handle.addEventListener("mousedown", down);
    handle.addEventListener("touchstart", down, { passive: false });
    document.addEventListener("mousemove", move);
    document.addEventListener("touchmove", move, { passive: false });
    document.addEventListener("mouseup", up);
    document.addEventListener("touchend", up);

    s.onMapMove = function () { place(s); };
    map.on("move zoom zoomend moveend resize", s.onMapMove);
    s._doc = { move: move, up: up };
    place(s);
  }

  function swipeOff(msg) {
    var s = state[msg.id];
    if (!s) return;
    if (s.map) {
      if (s.onMapMove) s.map.off("move zoom zoomend moveend resize", s.onMapMove);
      var lp = s.map.getPane(s.left); if (lp) lp.style.clip = "";
      var rp = s.map.getPane(s.right); if (rp) rp.style.clip = "";
    }
    if (s.divider && s.divider.parentNode) s.divider.parentNode.removeChild(s.divider);
    if (s._doc) {
      document.removeEventListener("mousemove", s._doc.move);
      document.removeEventListener("touchmove", s._doc.move);
      document.removeEventListener("mouseup", s._doc.up);
      document.removeEventListener("touchend", s._doc.up);
    }
    delete state[msg.id];
  }

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("nemetonSwipeOn", swipeOn);
    Shiny.addCustomMessageHandler("nemetonSwipeOff", swipeOff);
  }
})();
