# Management Group Profiles Configuration

Loads and exposes the configurable profiles used to classify UGF (Unités
de Gestion Forestière). The profile selected for a project determines: -
the dropdown label (e.g. "Groupe d'aménagement" for ONF, "Groupe de
gestion" for CRPF, "Zone" for OFB/ENS) - the list of allowed codes +
human-readable labels - the color palette used on the map - which codes
are considered "hors production" (excluded from production-oriented
indicators)

Profiles are defined in \`inst/config/groupes_amenagement.yaml\` and can
be overridden by a user-level file in
\`~/.local/share/nemeton/groupes_amenagement.yaml\` (or via the
\`nemeton.groupes_config\` option).
