# Migrera diagramskript till pxweb2r/rddiagram/rdverktyg

Den här guiden sammanfattar mönstret vi har använt för att migrera diagramskripten i det här
repot från den gamla stacken (`source()` mot `hamta_data`/`funktioner`-reporna, `pacman::p_load(tidyverse)`,
PxWeb v1 via `pxweb`) till den nya: helt paketbaserat, PxWeb v2 via `pxweb2r`, och
`rddiagram`/`rdverktyg` i stället för de gamla `source()`:ade funktionsfilerna.

Den är skriven utifrån ~40 genomförda migreringar i det här repot (`git log --oneline --all | grep pxweb2r`
listar dem) och är tänkt att kunna följas recept-mässigt på nästa skript.

## Varför migrera

- **Paket i stället för `source()` mot rå GitHub-URL.** De gamla skripten drog in kod med
  `source("https://raw.githubusercontent.com/Region-Dalarna/...")`, vilket är skört (inget
  versionshanterat beroende, ingen offline-möjlighet, svårt att testa). `rddiagram` och `rdverktyg`
  är riktiga R-paket i `Region-Dalarna/rdpaket`-repot.
- **PxWeb v2 i stället för v1.** SCB fasar ut v1 av PxWeb-API:et. `pxweb2r` pratar med v2.
- **Ingen `tidyverse`-attach.** Alla anrop görs med fullt namespace (`dplyr::filter()` osv.)
  i stället för `library(tidyverse)`/`p_load(tidyverse)`. Enklare att resonera om vilka funktioner
  som faktiskt används, inga krockande namn mellan paket.

## Steg-för-steg-recept

### 1. Byt ut paket-inladdningen i toppen av funktionen

Från:

```r
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, glue, pxweb)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_XXX_scb.R")
```

Till:

```r
if (!requireNamespace("rddiagram", quietly = TRUE)) {
  remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddiagram")
}
if (!requireNamespace("rdverktyg", quietly = TRUE)) {
  remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rdverktyg")
}
if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
# dplyr/purrr/stringr följer med som beroenden till rddiagram/rdverktyg - behöver inte laddas separat.
```

Om skriptet bara använder ett enstaka extra paket (t.ex. `glue`) - behåll ett litet
`if (!requireNamespace(...)) install.packages(...)` för det, men skriv om anropen i skriptet till
namespace (`glue::glue(...)`) i stället för att `library()`:a det.

### 2. Hitta rätt v2-tabell

Det gamla skriptet (eller `hamta_data`-funktionen det anropade) pekade mot en v1-URL, t.ex.
`https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning`. Motsvarande v2-tabell har
ett kort `TAB`-nummer (t.ex. `TAB3981`) - slå upp det i SCB:s v2-katalog eller fråga en kollega som
redan migrerat en tabell i samma familj. Skriv gärna en kort kommentar i skriptet om vilken v1-tabell
den ersätter och vad v2-tabellen heter, som spår för nästa person.

### 3. Ersätt `hamta_data`-anropet med `pxweb2r::pxweb2_get_data()`

Om `hamta_data`-funktionen bara användes av det här skriptet: lägg logiken direkt i skriptet som en
liten lokal hjälpfunktion (se exempel nedan) i stället för att migrera hela `hamta_data`-filen separat.
Om samma `hamta_data`-funktion delas av flera skript, migrera den för sig och låt skripten anropa den
migrerade versionen.

Minimalt exempel:

```r
px_df <- pxweb2r::pxweb2_get_data(
  table = "TAB3981",
  query = list(
    Region = region_vekt,
    Alder = alder_vekt,
    UtbildningsNiva = utbildningsniva_klartext,
    Kon = kon_klartext,
    ContentsCode = "UF0506A1",
    Tid = tid_vekt
  ),
  on_all_values_invalid = "null",
  quiet = TRUE
)
```

Viktiga saker att känna till om v2/`pxweb2r`:

- **Klartext eller kod** funkar båda som queryvärden (`Kon = c("män","kvinnor")` eller kodvärden),
  precis som i v1.
- **`"*"` betyder alla värden**, och `NA` betyder att variabeln elimineras (summeras bort) - kolla
  alltid om variabeln faktiskt är elimineringsbar i just den tabellen innan du förlitar dig på `NA`
  (se avsnittet om CKM nedan för ett fall där det INTE fungerade som väntat).
- **`on_all_values_invalid = "null"`** låter anropet returnera `NULL` i stället för att kasta fel om
  alla värden i en dimension (typ ett år) saknas i tabellen - användbart när du hämtar från två
  tabeller med olika årsspann (se CKM-avsnittet).
- **`quiet = TRUE`** tystar `pxweb2r`:s ofarliga informationsmeddelanden
  (`include_aggregations`-sammanfattningen, "latest period"-notiser, "ogiltiga värden borttagna"-notiser)
  utan att tysta faktiska fel/`warning()`. **Sätt alltid `quiet = TRUE`** på både
  `pxweb2_get_data()` och `pxweb2_get_values()`/`pxweb2_get_metadata()`-anrop - det är standard i alla
  migrerade skript i repot.
- Giltiga koder för en dimension (t.ex. vilka år som finns) hämtas med
  `pxweb2r::pxweb2_get_values("TAB3981", "Tid", quiet = TRUE)$code`.

### 4. Kolumnnamn efter hämtning skiljer sig från v1

- v2 ger alltid en **generisk `value`-kolumn** för måttet, oavsett tabellens klartext (till skillnad
  från v1 som döpte kolumnen efter tabellinnehållets klartext, t.ex. "Antal" eller "Befolkning").
  Byt namn på den själv direkt efter hämtningen, t.ex. `dplyr::rename(Befolkning = value)`.
  Gamla `if`-fix i skripten som bytte namn på en kolumn för att "SCB bytt namn på variabeln" (ofta med
  ett datumstämplat kommentarsspår, typ "2025-01-08 - ...") behövs oftast INTE längre - ta bort dem,
  men läs kommentaren först så du förstår vad den kompenserade för.
- Regionkolumnen heter `region_kod` i rådata från `pxweb2r` - byt till `regionkod` om resten av
  skriptet förväntar sig det namnet (`dplyr::rename(regionkod = region_kod)`).
  Klartextkolumnen med tabellens "vad detta mäter"-etikett heter `tabellinnehåll` - `dplyr::select(-tabellinnehåll)`
  bort den om du inte behöver den (t.ex. när `ContentsCode` bara har ett möjligt värde).

### 5. Skriv om pipe och tidyverse-anrop till namespace

- `%>%` → `|>` (native pipe).
- `filter()/mutate()/select()/rename()/group_by()/summarise()/ungroup()/relocate()/bind_rows()` →
  `dplyr::filter()` osv.
- `str_wrap()/str_remove()/str_trim()/str_to_sentence()` → `stringr::...`.
- `walk()/map()` → `purrr::...`.
- `.$kolumn` eller `%>% pull(x)` → `dplyr::pull(x)` (namespaced, fortfarande läsbart med `|>`).

### 6. Byt ut de gamla `func_SkapaDiagram.R`/`func_API.R`-funktionerna mot `rddiagram`/`rdverktyg`

De vanligaste motsvarigheterna vi stött på hittills:

| Gammalt (source()at) | Nytt |
|---|---|
| `SkapaStapelDiagram(...)` | `rddiagram::SkapaStapelDiagram(...)` |
| `diagramfarger("rus_sex")` | `rddiagram::diagramfarger("rus_sex")` |
| `skapa_kortnamn_lan(region, ...)` | `rdverktyg::skapa_kortnamn_lan(region, ...)` |
| `skapa_aldersgrupper(ålder, grupp)` | `rdverktyg::skapa_aldersgrupper(ålder, grupp)` |
| `manader_bearbeta_scbtabeller()` | `rdverktyg::manader_bearbeta_scbtabeller()` |
| `stop_tyst()` (i demo-blocket) | `rdverktyg::stop_tyst()` |
| `hamta_giltiga_varden_fran_tabell(url, "tid")` | `pxweb2r::pxweb2_get_values(tabell_id, "Tid", quiet = TRUE)$code` |

**OBS - `SkapaStapelDiagram()`-signaturen har ändrats något** i `rddiagram`-paketets nuvarande version:

- `utan_diagramtitel`-argumentet finns inte längre. Skicka `diagram_titel = NULL` direkt i stället
  när titeln ska bort.
- `diagram_facet`-argumentet finns inte kvar. Facet styrs enbart av om `facet_grp` är `NULL` eller
  ett kolumnnamn - gör det villkorat i skriptet, t.ex.
  `facet_grp = if (diag_facet) "region" else NULL`, för att bevara samma "fasetta bara om ett flaggargument
  är TRUE"-beteende som tidigare.

Kolla alltid `?rddiagram::SkapaStapelDiagram` (eller motsvarande diagramfunktion) mot det gamla
anropet innan du antar att alla argumentnamn är oförändrade - fler smärre namnbyten kan dyka upp
i andra diagramtyper (linje-, punkt-, kartdiagram osv.).

### 7. CKM-hantering (röjandekontroll) - när den gamla tabellen har fått en efterföljare

Flera SCB-tabeller har bytts ut mot en ny CKM-tabell (control of key marginals / röjandekontroll) från
och med årgång 2025, medan den gamla tabellen fortsätter täcka historiken fram till 2024. Mönstret för
de skripten:

```r
hamta_x <- function(table_id) {
  pxweb2r::pxweb2_get_data(
    table_id,
    query = list(Region = region_vekt, Kon = NA, Tid = tid),
    on_all_values_invalid = "null",
    quiet = TRUE
  )
}
x_hist <- hamta_x("TAB1234")   # gamla tabellen, t.ex. 2000-2024
x_ckm  <- hamta_x("TAB5678")   # nya CKM-tabellen, från 2025

har_ckm_data <- !is.null(x_ckm) && nrow(x_ckm) > 0
ckm_fran_ar  <- if (har_ckm_data) min(as.integer(x_ckm$år)) else NULL
diagram_capt <- rddiagram::lagg_till_ckm_notering(diagram_capt_bas, har_ckm_data, fran_ar = ckm_fran_ar)

x_df <- dplyr::bind_rows(x_hist, x_ckm)
```

Fallgropar vi har stött på i CKM-tabellerna:

- **`Kon = NA` (eliminera kön) fungerar inte alltid likadant i båda tabellerna.** I vissa CKM-tabeller
  är kön inte elimineringsbart på samma sätt, vilket ger en "totalkod"-komplikation - läs kommentarerna
  i redan migrerade CKM-skript (`git log --oneline --all | grep CKM`) för hur det löstes i det specifika
  fallet innan du antar att `NA` bara fungerar.
- **En "riktig" kategori kan finnas under flera koder i CKM-tabellen.** T.ex. har vissa CKM-tabeller
  åldersklassen "100+" och "totalt ålder" upprepade under flera olika koder (en per
  åldersklassificeringshierarki tabellen stödjer, typ 5-års-/10-årsklasser). Att skicka `Alder = "*"`
  och summera rakt av dubbelräknar då. Skriv en liten hjälpfunktion som hämtar metadata
  (`pxweb2r::pxweb2_get_metadata()` + `pxweb2r::pxweb2_get_values()`) och filtrerar fram exakt en kod
  per riktig kategori innan du hämtar data - se `diagram_flytt_inrikes_aldersgrupper_SCB.R` för ett
  fullständigt exempel (`hamta_giltiga_aldrar()`).
- **`on_all_values_invalid = "null"`** är nästan alltid rätt här, eftersom historik- och
  CKM-tabellen täcker olika årsspann - annars kastar anropet fel när du frågar efter ett år som inte
  finns i en av de två tabellerna.
- Testa alltid att summan över en dimension (t.ex. alla åldrar) för ett CKM-år ligger nära den
  officiella totalraden - CKM ger avsiktligt lite brus (avrundning för röjandekontroll), så en liten
  avvikelse (enstaka enheter) är förväntat och inget fel.

### 8. Kör `quiet = TRUE` på ALLA `pxweb2r`-anrop

Sätt `quiet = TRUE` på varenda `pxweb2r::pxweb2_get_data()`/`pxweb2_get_values()`/`pxweb2_get_metadata()`-anrop
i skriptet, inte bara det första. Annars läcker `include_aggregations`-sammanfattningar och liknande
brus ut i konsolen varje gång skriptet körs.

### 9. Testa mot skarp data innan commit

Kör funktionen end-to-end mot riktig SCB-data (inte bara att koden parsar) och jämför resultatet mot
det gamla skriptets utdata om möjligt:

- Rimliga och konsekventa värden över tid (särskilt vid CKM-övergången 2024→2025).
- Rätt antal diagram/facetter skapas.
- Länsnamn/etiketter ser ut som förväntat (t.ex. `rdverktyg::skapa_kortnamn_lan()`-resultat).
- Om skriptet har ett `demo = TRUE`-läge, kör det och kolla att det fortfarande fungerar.

## Checklista för en migrering

- [ ] `p_load(tidyverse, ...)` och `source()`-rader mot `funktioner`/`hamta_data`-reporna borttagna.
- [ ] `rddiagram`/`rdverktyg`/`pxweb2r` laddas med `requireNamespace()` + `remotes::install_github()`.
- [ ] Rätt v2-`TAB`-nummer identifierat (kommentar i skriptet om vilken v1-tabell det ersätter).
- [ ] `hamta_data`-anropet ersatt med `pxweb2r::pxweb2_get_data()` (lokal hjälpfunktion om bara detta
      skript använde den gamla `hamta_data`-funktionen).
- [ ] Kolumnnamn efter hämtning kollade (`value`→ rätt namn, `region_kod`→`regionkod`,
      `tabellinnehåll` borttagen om onödig). Gamla omdöpningsfix för "SCB bytte kolumnnamn"-buggar
      borttagna om de inte längre behövs.
- [ ] `%>%` → `|>`, tidyverse-anrop namespacade (`dplyr::`/`stringr::`/`purrr::`).
- [ ] Diagramanrop (`SkapaStapelDiagram` m.fl.) uppdaterade mot `rddiagram`:s nuvarande signatur
      (`utan_diagramtitel`/`diagram_facet` finns inte kvar).
- [ ] CKM-hantering genomgången om tabellen har en efterföljare (se avsnitt 7) - `Kon = NA`,
      dubbelkodade kategorier, `on_all_values_invalid = "null"`.
- [ ] `quiet = TRUE` på samtliga `pxweb2r`-anrop.
- [ ] Testat end-to-end mot skarp SCB-data, jämfört rimlighet mot gamla skriptet.
- [ ] Commit-meddelandet beskriver: vilken v1→v2-tabell det gäller, ev. CKM-mönster, vad som testats.

## Referenser

- Paketen: `Region-Dalarna/rdpaket` (innehåller `rddiagram` och `rdverktyg`), `FaluPeppe/pxweb2r`.
- Exempel att titta på i det här repot (`git show <hash>` för fullständig diff + commit-beskrivning):
  - Enkel migrering, en tabell: `a3dc45b` (`diag_utbniva_flera_diagram_scb.R`).
  - CKM med två tabeller, enkelt fall (kön elimineringsbart i båda): `ed33011`
    (`diagram_inflyttlan_utflyttlan_SCB.R`).
  - CKM med dubbelkodade ålderskategorier: `8cd5979` (`diagram_flytt_inrikes_aldersgrupper_SCB.R`).
  - Massmigrering av `quiet = TRUE` på 153 anrop i 67 filer: `8d8adfb`.
  - Tystande av kvarvarande brus + dödkodsstädning: `aea5261`.
  - Full lista: `git log --oneline --all | grep -i pxweb2r` i det här repot.
