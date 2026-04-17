# Domain Layer: Unités de Gestion (UG)

Pure domain logic for Management Units (UG). Contains constructors,
commands, queries, and invariant validation. No Shiny dependency.

Domain concepts: - Parcelle cadastrale: immutable cadastral polygon -
Tenement: smallest geometric subdivision (whole parcel or fragment) -
UG: grouping of 1+ tenements = operational forestry unit - Groupe
d'aménagement: functional classification of UGs

Each command returns a new projet (functional immutability). All 5
invariants are enforced after every command.
