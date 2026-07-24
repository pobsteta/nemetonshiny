# Theia / DATA TERRA data source integration

Application-level orchestration for the Theia / DATA TERRA data sources
exposed by the nemeton core package (\>= 0.40.0). This service only
\*loads\* and \*forwards\* rasters to the nemeton indicator functions;
it never computes business logic itself.

The Theia STAC API is queried by \`nemeton\`, which signs asset URLs
internally through its STAC gateway (\`signing.stac.teledetection.fr\`,
pure R) and reads them with /vsicurl/. Access requires a registered API
key (\`TLD\_\*\` env vars). Python/reticulate is no longer used for
THEIA URL signing (it remains required by other core paths — FORDEAD /
RECONFORT).
