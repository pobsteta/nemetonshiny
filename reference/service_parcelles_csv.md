# Import a project from a CSV list of cadastral references

Creates a project from a one-line CSV holding the cadastral parcel
references of a forest, e.g. \`A1;A2;A3;...;AO212;AO220\`.

\*\*The commune is read from the FILE NAME\*\*, by convention
\`commune-code_insee.csv\` (\`couchey-21200.csv\`). The file's content
carries no commune at all, so nothing else could tell which cadastre the
references belong to - \`A1\` exists in most communes of France.

The references are the short form a forester writes: section letters
followed by the parcel number, without the leading zeros the cadastre
stores (\`212000000A0036\`). Matching therefore happens on the \*pair\*
(section, numero), never on the raw identifier.
