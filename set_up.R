library(data.table)
library(sf)
# Helper function(s) ===========================================================
in_repeater <- function(.field, .values) {
  temp <- sapply(.values, function(x) {
    paste0("%27", x, "%27,")
  })
  temp <- paste0(unlist(temp), collapse ="")
  paste0(.field, "%20in%20(", substr(temp, 1, nchar(temp) - 1), ")")
}
get_extended_esca_ownership <- function(.df, .coe) {
  sapply(.df[[.coe]], function(y) {
    if (length(unlist(strsplit(.df[.df[[.coe]] == y, ][["land_holder"]], ","))) > 1) {
      paste0("<details><summary>",
             .df[.df[[.coe]] == y, ][["land_holder"]],
             "</summary><p>",
             .df[.df[[.coe]] == y, ][["land_holder_notes"]],
             "</p></details>")
    } else {
      paste0(.df[.df[[.coe]] == y, ][["land_holder"]])
    }
  })
}
# Get data =====================================================================
fo <- sf::st_read("https://maps.fodis.net/server/rest/services/OpenData/AdministrativeBoundaries/FeatureServer/4/query?where=1%3D1&outFields=*&outSR=4326&f=json")

ra <- sf::st_read("https://maps.fodis.net/server/rest/services/OpenData/AdministrativeBoundaries/FeatureServer/1/query?where=parcel_id='F1.13'&outFields=*&outSR=4326&f=json")
wanted <- data.frame("coe"          = c("E11b.6.1", "E11b.7.1.1", "E11b.8", "E18.1.1", "E18.1.2", "E18.1.3", "E18.4", "E19a.1", "E19a.2", "E19a.3", "E19a.4", "E19a.5", "E20c.2", "E21b.3", "E23.1", "E23.2", "E24", "E29.1", "E34", "E38", "E39", "E40", "E41", "E42", "F1.7.2", "L20.13.1.2", "L20.13.3.1", "L20.18", "L20.19.1.1", "L20.2.1", "L20.3.1", "L20.3.2", "L20.5.1", "L20.5.2", "L20.5.3", "L20.5.4", "L23.2", "L32.1", "L5.7", "L6.2", "S1.3.2"),
                     "mra"          = c("Future East Garrison", "Future East Garrison", "Future East Garrison", "Parker Flats", "Parker Flats", "Parker Flats", "Parker Flats", "Parker Flats", "Parker Flats", "Parker Flats", "Parker Flats", "Parker Flats", "Parker Flats", "Parker Flats", "Seaside", "Seaside", "Seaside", "DRO / Monterey", "Seaside", "Interim Action Ranges", "Interim Action Ranges", "Interim Action Ranges", "Interim Action Ranges", "Interim Action Ranges", "MOUT", "DRO / Monterey", "DRO / Monterey", "Parker Flats", "Future East Garrison", "County North", "Laguna Seca Parking", "Laguna Seca Parking", "Laguna Seca Parking", "Laguna Seca Parking", "Laguna Seca Parking", "Laguna Seca Parking", "Parker Flats", "Parker Flats", "County North", "DRO / Monterey", "CSUMB Off-Campus"),
                     "group"        = c("Group 4", "Group 4", "Group 4", "Group 1", "Group 1", "Group 1", "Group 1", "Group 1", "Group 1", "Group 1", "Group 1", "Group 1", "Group 1", "Group 1", "Group 1", "Group 1", "Group 1", "Group 3", "Group 1", "Interim Action Ranges", "Interim Action Ranges", "Interim Action Ranges", "Interim Action Ranges", "Interim Action Ranges", "Group 3", "Group 3", "Group 3", "Group 1", "Group 4", "Group 2", "Group 3", "Group 3", "Group 3", "Group 3", "Group 3", "Group 3", "Group 1", "Group 1", "Group 2", "Group 3", "Group 2"),
                     "jurisdiction" = c("County of Monterey", "County of Monterey", "County of Monterey", "City of Seaside", "County of Monterey", "City of Seaside", "City of Seaside", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "City of Seaside", "County of Monterey", "City of Seaside", "City of Seaside", "City of Seaside", "City of Monterey", "City of Seaside", "City of Seaside", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "City of Del Rey Oaks", "City of Del Rey Oaks", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "City of Del Rey Oaks", "County of Monterey"),
                     "land_holder"  = c("County of Monterey", "County of Monterey", "County of Monterey", "City of Seaside", "County of Monterey", "City of Seaside", "City of Seaside", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "MPC", "City of Seaside", "MPC", "City of Seaside", "City of Seaside", "City of Seaside", "City of Monterey", "City of Seaside", "MPC", "MPC", "MPC", "MPC", "MPC", "MPC", "City of Del Rey Oaks", "City of Del Rey Oaks", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "County of Monterey", "MPRPD", "CSUMB"),
                     "lucip"        = c("https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0364B//ESCA-0364B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0364B//ESCA-0364B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0364B//ESCA-0364B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0337B//ESCA-0337B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0337B//ESCA-0337B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0337B//ESCA-0337B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0337B//ESCA-0337B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0337B//ESCA-0337B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0364B//ESCA-0364B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0305B//ESCA-0305B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0361E//ESCA-0361E.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0305B//ESCA-0305B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0301B//ESCA-0301B.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0305B//ESCA-0305B.pdf"),
                     "rod"          = c("https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0360//ESCA-0360.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0360//ESCA-0360.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0360//ESCA-0360.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0331//ESCA-0331.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0331//ESCA-0331.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0331//ESCA-0331.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0331//ESCA-0331.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0331//ESCA-0331.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0360//ESCA-0360.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0298/ESCA-0298.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0359//ESCA-0359.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0298/ESCA-0298.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0293/ESCA-0293.pdf", "https://docs.fortordcleanup.com/ar_pdfs/AR-ESCA-0298/ESCA-0298.pdf"))

api_url <- "https://maps.fodis.net/server/rest/services/OpenData/AdministrativeBoundaries/FeatureServer/1/query?"
esca <- sf::st_read(paste0(api_url, 
                           "where=",
                           in_repeater(.field = "FortOrd_DBO_tblParcel_COENumber",
                                       .values = wanted[["coe"]]),
                           "&outFields=*", 
                           "&outSR=4326&f=json")) |>
  merge(x = wanted, by.y = "FortOrd_DBO_tblParcel_COENumber", by.x = "coe") |>
  st_as_sf()
# Additional tuneups -----------------------------------------------------------
esca[["land_holder_notes"]] <- ""
esca[["land_holder"]] <- ifelse(esca[["coe"]] == "E18.1.1",
                                "City of Seaside, CalVet",
                                esca[["land_holder"]])
esca[["land_holder"]] <- ifelse(esca[["coe"]] == "E18.1.2",
                                "County of Monterey, CalVet",
                                esca[["land_holder"]])
esca[["land_holder_notes"]] <- ifelse(esca[["coe"]] == "E18.1.1",
                                      "The central area in this parcel, approximately between Parker Flats Road and Parker Flats Cut-Off, is owned by CalVet; otherwise, owned by the City of Seaside.",
                                      esca[["land_holder_notes"]])
esca[["land_holder_notes"]] <- ifelse(esca[["coe"]] == "E18.1.2",
                                      "The central area in this parcel, approximately between Parker Flats Road and Parker Flats Cut-Off, is owned by CalVet; otherwise, owned by the County of Monterey.",
                                      esca[["land_holder_notes"]])
esca[["land_holder"]] <- ifelse(esca[["coe"]] == "E18.1.3",
                                "City of Seaside, AP Glover Enterprises LLC",
                                esca[["land_holder"]])
esca[["land_holder_notes"]] <- ifelse(esca[["coe"]] == "E18.1.3",
                                      "AP Glover Enterprises LLC owns the approximately 4.98 acre Medical Officer Barracks site. The rest of the parcel is owned by the City of Seaside.",
                                      esca[["land_holder_notes"]])
esca[["mra_popup"]] <- paste0("<table>",
                              ### COE Field ------------------------------------
                              "<tr><th><b>COE Id: </b></th><td>",
                              esca[["coe"]], "</td></tr>",
                              ### Parcel Owner Field ---------------------------
                              "<tr><th><b>Land Owner(s): </b></th><td>",
                              get_extended_esca_ownership(.df = esca, .coe = "coe"),
                              "</td></tr>",
                              ### Army ROD Field -------------------------------
                              "<tr><th><b>ROD: </b></th><td>",
                              paste0("<a href='", esca[["rod"]], "' target ='_blank, id = 'link''>Here</a>"), 
                              "</td></tr>",
                              ### LUCIP/OMP Field ------------------------------
                              "<tr><th><b>LUCIP/OMP: </b></th><td>",
                              paste0("<a href='", esca[["lucip"]], "' target ='_blank, id = 'link''>Here</a>"), 
                              "</td></tr>",
                              "</table>")
## Signage ---------------------------------------------------------------------
gateways <- data.frame(
  name = c(
    # COS
    "Coe and GJMB", "Parker Flats Road", "Parker Flats Cut Off Road",
    "Parker Flats Cut Off Road",
    # MOCO
    "Parker Flats Road",
    "8th and Giggling 1", "8th and Giggling 2", "8th and Giggling 3",
    "Jerry Smith and Inter-Garrison", "Inter-Garrison",
    "West Camp and Inter-Garrison", "Barloy Canyon", "Barloy Canyon and Trail 50",
    # CSUMB
    "8th", "Inter-Garrison 1", "Inter-Garrison 2", "Inter-Garrison 3", "Inter-Garrison 4"
  ),
  jurisdiction = c(
    # COS
    "COS", "COS", "COS",
    "COS", 
    # MOCO
    "MOCO",
    "MOCO", "MOCO", "MOCO", 
    "MOCO", "MOCO",
    "MOCO", "MOCO", "MOCO",
    # CSUMB
    "CSUMB", "CSUMB", "CSUMB", "CSUMB", "CSUMB" 
  ),
  lat  = c(
    # COS
    36.621491, 36.637419,  36.636610,
    36.639727, 
    # MOCO
    36.638896, 
    36.643462, 36.643329, 36.643493, 
    36.655309, 36.655412, 
    36.651261, 36.644026, 36.633189, 
    # CSUMB
    36.649455, 36.654560, 36.654594, 36.654857, 36.654890
  ),
  lon  = c(
    # COS
    -121.816935, -121.796371, -121.79834,
    -121.798528, 
    # MOCO
    -121.79172,
    -121.787673, -121.787115, -121.786766,
    -121.757936, -121.748820, 
    -121.747330, -121.730626, -121.735757,
    # CSUMB
    -121.786622, -121.784548, -121.782094, -121.776586, -121.773236),
  cat = "Usage Control Sign", icon = "info")
closed <- data.frame(
  name = c("Monterey MRA", "Monterey MRA", "Monterey MRA", "Monterey MRA",
           "MOUT", "MOUT", "MOUT", "MOUT",
           "Future East Garrison"),
  jurisdiction = c("COM", "COM", "COM", "COM",
                   "MPC", "MPC", "MPC", "MPC",
                   "MOCO"),
  lat  = c(36.589848, 36.588285, 36.588187, 36.58612,
           36.621742, 36.621048, 36.618469, 36.61588,
           36.64057),
  lon  = c(-121.825557, -121.823304, -121.827591, -121.824484,
           -121.748838, -121.749864, -121.75445, -121.756928,
           -121.735231),
  cat = "Restricted Area Sign", icon = "restricted")
gates <- data.frame(
  name = c("Eucalyptus Road - Gate 1", "Paker Flats Cutoff Road - Gate 1",
           "Parker Flats Road - Gate 1", "8th Avenue Extension - Gate 1",
           "Gigling Road - Gate 1", "Gigling Road - Gate 2", 
           "Watkins Gate Road - Gate 1", "Watkins Gate Road - Gate 2",
           "Barloy Canyon Road - Gate 1", "Cresent Bluff Road - Gate 1",
           "Barloy Canyon Road - Gate 2", "South Boundary Road - Gate 1",
           "Impossible Canyon South Gate", # Ask Betsy
           "Barloy Canyon Road Gate 3",
           "GJMB Gate 1", "GJMB Gate 2", "GJMB Gate 3", "Blue Line Gate"),
  jurisdiction = c("MOCO", "COS",
                   "COS", "MOCO",
                   "MOCO", "MOCO",
                   "MOCO", "MOCO",
                   "MOCO", "MOCO",
                   "MOCO", "MOCO",
                   "MOCO",
                   "MOCO",
                   "COS", "COS", "COS", "COS"),
  lat  = c(36.62952, 36.635735, 
           36.63786, 36.643302, 
           36.643541, 36.643437,
           36.65101, 36.65092,
           36.649803, 36.647442,
           36.624422, 36.585594,
           36.586061,
           36.589382,
           36.614670, 36.609714, 36.600471, 36.627099),
  lon  = c(-121.791962, -121.7973, 
           -121.793841, -121.78706,
           -121.786886, -121.784931,
           -121.744676, -121.741460,
           -121.731947, -121.731581,
           -121.742066, -121.761653,
           -121.760641,
           -121.752686,
           -121.819669, -121.822165, -121.826696, -121.798062),
  cat = "Gate Access Signage", icon = "gate")
# Fire Lane --------------------------------------------------------------------
fire_lanes <- data.frame(
  name = c("Gigling Road - Gate 1",
           "8th Avenue Extension - Gate 1",
           "Parker Flats Cutoff Road - Gate 1",
           "Parker Flats Road - Gate 1"),
  jurisdiction = c("MOCO", "MOCO", "COS", "COS"),
  lat = c(36.643309, 36.643543, 36.635700, 36.637889),
  lon = c(-121.787158, -121.786900, -121.797285, -121.793845),
  cat = "Fire Lane",
  icon = "parking"
)
# Signs Joined ----------------------------------------------------------------- 
signs <- rbind(gateways, closed, gates, fire_lanes) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  setDT()
signs <- signs[, c("color", "jurisdiction", "content", "Latitude", "Longitude") := .(
  fcase(jurisdiction == "COM"   , "#046B33;",
        jurisdiction == "COS"   , "#4C94AC;",
        jurisdiction == "MOCO"  , "#0E86D4;",
        jurisdiction == "CSUMB" , "#8B784A;",
        jurisdiction == "MPC"   , "#860038;"),
  fcase(jurisdiction == "COM"   , "City of Monterey",
        jurisdiction == "COS"   , "City of Seaside",
        jurisdiction == "MOCO"  , "Monterey County",
        jurisdiction == "CSUMB" , "CSUMB",
        jurisdiction == "MPC"   , "MPC"),
  fcase(cat          == "Gate Access Signage",
        "<ul>
          <li>Jurisdiction logo and name</li>
          </br>
          <li>Gate Name</li>
          </br>
          <li>Restrictions (e.g., Authorized Vehicles Only)</li>
          </br>
          <li>Access Information (e.g., For access call (831) 394-6811)</li>
        </ul>",
        cat          == "Restricted Area Sign",
        "<ul>
          <li>Jurisdiction logo and name</li>
          </br>
          <li>Restrictions (e.g., NO TRESSPASSING Violators will be prosecuted)</li>
          </br>
          <li>Jurisdiction Code (e.g., City of Monterey Code, Monterey County Code)</li>
        </ul>",
        cat          == "Usage Control Sign",
        "<ul>
          <li>Jurisdiction logo, name and contact information</li>
          </br>
          <li>Restrictions (e.g., use restricted to signed trails)</li>
          </br>
          <li>Allowed or prohibited use symbols</li></br><li>Additional information (e.g., QR code and 3Rs)</li>
        </ul>",
        cat          == "Fire Lane",
        "<ul>
          <li>Signs or markings including the words 'No Parking - FIRE LANE' per California Fire Code, Chapter 5, Section 503.3</li>
        </ul>"),
  st_coordinates(geometry)[, "X"],
  st_coordinates(geometry)[, "Y"])] |>
  sf::st_as_sf()
