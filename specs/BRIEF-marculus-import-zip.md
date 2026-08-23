# BRIEF `marculus` — réceptionner un lot de chantiers (.zip) produit par Nemeton

> **Statut** : ouvert, 2026-08-23.
> **Dépôt concerné** : `marculus` (application Android).
> **Émetteur** : `nemetonshiny` v0.136.1, export Marculus.
> **Nature** : une nouvelle porte d'entrée. Aucun changement de modèle, aucune
> migration Room.
> **Ce qui existe déjà côté Nemeton** : le lot est produit et téléchargeable ;
> le champ qui permet l'appariement est **déjà émis** (§3). Rien à attendre de
> ce côté-là pour commencer.

---

## Le problème, tel qu'il se pose sur le terrain

Nemeton produit désormais, pour un projet forestier, un **lot de chantiers de
martelage** : un ZIP contenant un GeoPackage par chantier et un fichier
`.marsync` portant tous les contextes. Sur un projet réel — forêt de
ForêtAccess, 30 parcelles — cela fait **13 contextes, 13 GeoPackages, 12 Mo**.

Aujourd'hui, la réception se fait en deux temps :

1. ouvrir le `.marsync` → les 13 contextes se créent par fusion. **Ça marche.**
2. pour chaque contexte, **rattacher son GeoPackage à la main**.

C'est l'étape 2 qui pose problème : **treize rattachements manuels**, dans un
sélecteur de fichiers, à partir de treize noms qui se ressemblent
(`ForetAccess_-_ug_1_-_eclaircie.gpkg`, `..._ug_10_-_eclaircie.gpkg`…). Une
erreur d'appariement ne se voit pas : le contexte s'ouvre, la carte affiche une
parcelle — la mauvaise — et les tiges se rattachent spatialement à un périmètre
qui n'est pas le leur. Le journal est alors faux sans jamais avoir été en erreur.

## Ce qui marche déjà, et qu'il ne faut pas casser

Lu dans `ParametresScreen.kt` le 2026-08-23, pour que ce brief parle du code
réel et non d'un code supposé.

**Le `.marsync` s'importe dès aujourd'hui.** Le bouton *Fusionner* (l. 292)
ouvre `OpenDocument()` sur `arrayOf("*/*")` — n'importe quel fichier — le lit
comme du texte et appelle `fusionnerJson()`. Un opérateur qui décompresse le lot
sur son téléphone crée donc ses treize contextes sans qu'une ligne change. Ce
qui reste manuel, ce sont les treize rattachements de GeoPackage.

**La plomberie ZIP existe.** `lireJsonDepuisZip()` (l. 402) sait déjà parcourir
un `ZipInputStream`. Ce brief ne demande donc pas d'introduire la lecture
d'archive, mais d'en ajouter un **second usage**.

**Et c'est là qu'est le danger.** Ce lecteur cherche une entrée au nom figé,
`marculus.json` (`ENTREE_ZIP`), et remet son contenu à **`importerJson()`** — la
restauration destructive. Un lot Nemeton n'en contient pas : il passerait donc
sans rien faire, ce qui est le bon échec. Mais les deux imports vivraient dans
le même écran, à quelques lignes l'un de l'autre — *Restaurer*, qui efface tout,
et *Importer un lot*, qui fusionne. **Ne pas réutiliser `lireJsonDepuisZip()`
tel quel** : son nom ne dit pas qu'il alimente la porte destructive.

**Aucun intent-filter hors `LAUNCHER`** (`AndroidManifest.xml`). On ne peut donc
pas « ouvrir avec Marculus » depuis un gestionnaire de fichiers ni depuis le
partage système : tout passe par les sélecteurs internes. Ce n'est pas
bloquant — le sélecteur suffit — mais un filtre sur l'extension du lot
raccourcirait le geste. À arbitrer séparément.

## Ce qui est demandé

Une entrée **« Importer un lot (.zip) »**, à côté de l'import de sauvegarde
existant, qui fasse en un geste ce que l'opérateur fait treize fois.

```
lot.zip
├── ForetAccess.marsync                        ← les 13 contextes
├── ForetAccess_-_ug_1_-_eclaircie.gpkg
├── ForetAccess_-_ug_2_-_eclaircie.gpkg
└── …                                          ← un par contexte
```

Déroulé proposé :

1. décompresser dans un répertoire temporaire ;
2. lire l'unique `*.marsync` → **`fusionnerJson()`** ;
3. pour chaque contexte, lire son champ **`gpkgNom`** ; si le fichier de ce nom
   est présent dans le lot, le copier dans le stockage privé et renseigner
   `cheminGpkg` ;
4. nettoyer le temporaire.

## §3 — `gpkgNom` : le champ qui apparie, et il est déjà là

Chaque contexte du `.marsync` porte désormais :

```json
{
  "id": "…",
  "nom": "ForetAccess - ug_1 - eclaircie",
  "gpkgNom": "ForetAccess_-_ug_1_-_eclaircie.gpkg",
  …
}
```

**C'est une clé inconnue de toutes les versions publiées**, et c'est
délibéré : `JSONObject` ignore ce qu'il ne lit pas, donc `versContexte()`
continue de fonctionner à l'identique sur les versions actuelles. Le champ est
**inerte jusqu'à ce que vous le lisiez** — aucune version à synchroniser, aucun
ordre de livraison à respecter.

Deux propriétés sur lesquelles vous pouvez compter :

- c'est un **nom de fichier nu**, jamais un chemin : le lot est à plat, et
  `basename()` est appliqué à l'émission ;
- il est **ASCII** — ni accent, ni espace, ni caractère spécial — pour traverser
  un ZIP et un système de fichiers Android sans surprise.

`cheminGpkg`, lui, reste **absent** du fichier émis : c'est un chemin dans votre
stockage privé, que la machine émettrice ne peut pas connaître. C'est à
l'import de le renseigner, après copie.

## Ce qui doit rester vrai

**La fusion, jamais la restauration.** `fusionnerJson()` fait une union par
UUID, non destructive : les tiges déjà saisies survivent. `importerJson()`
**efface** contextes, tiges et configs avant d'insérer. Les deux formats ne
diffèrent que par la présence d'une section `referentiels` — que le lot n'émet
pas, précisément pour qu'il ne puisse pas être confondu avec une sauvegarde.
Si l'import de lot pouvait atterrir sur `importerJson()`, il détruirait une
journée de martelage sans avertissement.

**Zip-slip.** Une entrée d'archive dont le nom contient `..` ou un chemin absolu
doit être **refusée**, pas assainie. Le lot légitime est plat : toute entrée
avec un séparateur de chemin est déjà anormale.

**Un lot partiel reste utile.** Si un `gpkgNom` désigne un fichier absent, créer
le contexte **sans** GeoPackage plutôt que de rejeter tout le lot : douze
chantiers importés valent mieux que zéro, et l'opérateur peut rattacher le
treizième à la main. Le dire, en revanche — un contexte muet dont on ne sait pas
pourquoi il n'a pas de carte est pire qu'un message.

**Idempotence.** Ré-importer le même lot ne doit rien dupliquer : les `id` de
contexte sont stables (ils viennent de l'action du plan Nemeton), donc la
fusion par UUID couvre déjà le cas. Reste la copie du GeoPackage : écraser plutôt
qu'accumuler `fichier(1).gpkg`.

## Ce que contient un GeoPackage du lot

Conforme à `docs/specs/couches-gpkg.md`, vérifié à l'émission :

| Table | Contenu | Note |
|---|---|---|
| `parcelle` | périmètre de l'UGF du chantier | colonnes `proprietaire`, `foret`, `commune`, `section`, `numero` |
| `desserte` | routes, pistes, chemins | colonne `type` : `existante`, `piste_creee`, `osm`, `detectee` |

**Pas de table de tuiles** : l'orthophoto du projet pèse des gigaoctets, un fond
de carte qui ne tient pas sur le téléphone n'est pas un fond. Si un fond
hors-ligne devient nécessaire, il faudra une pyramide réduite — sujet à ouvrir
séparément.

**Pas de couche `houppier`** pour l'instant : la segmentation de couronnes sur
MNH est en cours côté cœur Nemeton (`specs/BRIEF-nemeton-houppiers-mnh.md`).
Elle arrivera dans le même GeoPackage, sous le nom `houppier`, sans rien changer
au format du lot.

## Vérification proposée

| Contrôle | Attendu |
|---|---|
| Lot de 13 chantiers (12 Mo) | 13 contextes créés, 13 GeoPackages rattachés, aucun geste manuel |
| Ré-import du même lot | aucun doublon, aucun `fichier(1).gpkg` |
| Lot amputé d'un GeoPackage | 12 rattachés, le 13ᵉ créé sans carte, et l'écart signalé |
| ZIP sans `.marsync` | refus explicite, rien créé |
| Entrée d'archive contenant `..` | refus, pas d'assainissement silencieux |
| Tiges déjà saisies dans un contexte ré-importé | intactes |

## Question ouverte, si l'usage le demande

Le lot est aujourd'hui **téléchargé** depuis Nemeton, puis transféré au
téléphone (Quick Share, cloud, câble — ou directement, si l'opérateur ouvre
Nemeton dans le navigateur du téléphone, ce qui est le chemin le plus court).
Un jour, une récupération par URL éviterait le transfert. Ça demanderait côté
Nemeton une URL de lot signée et à durée limitée : un chantier en soi, à ouvrir
seulement si le transfert manuel s'avère être le point de friction — ce que
l'usage dira.
