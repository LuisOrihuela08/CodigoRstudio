# Instalar y cargar los paquetes
install.packages("igraph")
install.packages("leaflet")
install.packages("geosphere")

library(igraph)
library(leaflet)
library(geosphere)

# Definir las ciudades
ciudades <- c("Hanoi", "Ho Chi Min", "Vinh", "Da Nang", "Nha Trang", "Siem Reap")

# Definir las distancias entre las ciudades
distancias <- data.frame(
  from = c("Vinh", "Vinh", "Vinh", "Vinh", "Vinh", "Ho Chi Min", "Ho Chi Min", "Ho Chi Min", "Ho Chi Min", 
           "Siem Reap", "Siem Reap", "Siem Reap", "Nha Trang", "Nha Trang", "Da Nang"),
  to = c("Hanoi", "Da Nang", "Nha Trang", "Siem Reap", "Ho Chi Min", "Hanoi", "Da Nang", "Nha Trang", "Siem Reap", 
         "Hanoi", "Da Nang", "Nha Trang", "Hanoi", "Da Nang", "Hanoi"),
  peso = c(275, 395, 1001, 900, 300, 1161, 992, 345, 388, 889, 580, 990, 1559, 466, 629)
)

# Crear el grafo con las distancias como pesos
g <- graph_from_data_frame(distancias, vertices = data.frame(name = ciudades), directed = FALSE)

# Verificar y asignar los pesos correctamente
E(g)$weight <- distancias$peso

# Función para verificar si una secuencia de vértices es un camino Hamiltoniano
is_hamiltonian_path <- function(graph, path) {
  n <- length(V(graph))
  if (length(path) != n) return(FALSE)
  
  for (i in 1:(n-1)) {
    if (!are_adjacent(graph, path[i], path[i+1])) return(FALSE)
  }
  return(TRUE)
}

# Función para encontrar caminos Hamiltonianos usando backtracking
find_hamiltonian_paths <- function(graph, current_path) {
  if (length(current_path) == length(V(graph))) {
    return(list(current_path))
  }
  
  paths <- list()
  for (neighbor in neighbors(graph, tail(current_path, n=1))) {
    if (!(neighbor %in% current_path)) {
      new_path <- c(current_path, neighbor)
      paths <- c(paths, find_hamiltonian_paths(graph, new_path))
    }
  }
  return(paths)
}

# Función para calcular la distancia total de un camino Hamiltoniano
calculate_path_distance <- function(graph, path) {
  total_distance <- 0
  for (i in 1:(length(path) - 1)) {
    edge_id <- get.edge.ids(graph, c(path[i], path[i+1]))
    total_distance <- total_distance + E(graph)[edge_id]$weight
  }
  return(total_distance)
}

# Iniciar la búsqueda desde cada vértice
hamiltonian_paths <- list()
for (start_vertex in V(g)) {
  hamiltonian_paths <- c(hamiltonian_paths, find_hamiltonian_paths(g, c(start_vertex)))
}

# Filtrar y mostrar todos los caminos Hamiltonianos encontrados
hamiltonian_paths <- Filter(function(path) is_hamiltonian_path(g, path), hamiltonian_paths)

# Función para verificar si un camino es único (no duplicado)
is_unique_path <- function(path, path_list) {
  for (p in path_list) {
    if (all(path == p)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# Filtrar caminos Hamiltonianos únicos
unique_hamiltonian_paths <- list()
for (path in hamiltonian_paths) {
  if (is_unique_path(path, unique_hamiltonian_paths)) {
    unique_hamiltonian_paths <- c(unique_hamiltonian_paths, list(path))
  }
}
print(unique_hamiltonian_paths)

# Calcular la distancia total de cada camino Hamiltoniano único
path_distances <- sapply(unique_hamiltonian_paths, function(path) calculate_path_distance(g, path))

# Encontrar el camino con la menor distancia total
min_distance_index <- which.min(path_distances)
min_distance_path <- unique_hamiltonian_paths[[min_distance_index]]
min_distance <- path_distances[min_distance_index]

# Imprimir resultados
print("Caminos Hamiltonianos únicos y sus distancias:")
for (i in 1:length(unique_hamiltonian_paths)) {
  print(paste("Camino:", paste(V(g)[unique_hamiltonian_paths[[i]]]$name, collapse = " -> "), 
              "Distancia total:", path_distances[i]))
}

print(paste("El camino con la menor distancia es:", paste(V(g)[min_distance_path]$name, collapse = " -> "), 
            "con una distancia de", min_distance, "km"))

# Visualización del grafo sin el camino Hamiltoniano resaltado
plot(g, vertex.label = V(g)$name, vertex.size=30, vertex.color="lightblue",
     edge.color="gray", edge.width=1, 
     edge.label = E(g)$weight,  # Mostrar los pesos en las aristas
     main="Grafo de las Ciudades y sus Distancias")

# Visualización del grafo y del camino Hamiltoniano con la menor distancia
if (length(unique_hamiltonian_paths) > 0) {
  print("Se encontró al menos un camino Hamiltoniano.")
  print(min_distance_path)
  
  # Resaltar el camino Hamiltoniano con la menor distancia encontrado
  E(g)$color <- "gray"
  E(g)$width <- 1
  
  for (i in 1:(length(min_distance_path) - 1)) {
    edge_id <- get.edge.ids(g, c(min_distance_path[i], min_distance_path[i+1]))
    E(g)[edge_id]$color <- "red"
    E(g)[edge_id]$width <- 2
  }
  
  # Resaltar los vértices en el camino Hamiltoniano con la menor distancia
  V(g)$color <- ifelse(V(g) %in% min_distance_path, "red", "lightblue")
  
  # Visualizar el grafo con los pesos de las aristas y el camino Hamiltoniano resaltado
  plot(g, vertex.label = V(g)$name, vertex.size=30, vertex.color=V(g)$color,
       edge.color=E(g)$color, edge.width=E(g)$width, 
       edge.label = E(g)$weight,  # Mostrar los pesos en las aristas
       main="Camino Hamiltoniano con la Menor Distancia")
} else {
  print("No se encontró ningún camino Hamiltoniano.")
}

# Definir las coordenadas geográficas de las ciudades
ciudades_coord <- data.frame(
  name = c("Hanoi", "Ho Chi Min", "Vinh", "Da Nang", "Nha Trang", "Siem Reap"),
  lat = c(21.0285, 10.8231, 18.6796, 16.0471, 12.2388, 13.3671),
  lon = c(105.8542, 106.6297, 105.6763, 108.2068, 109.1967, 103.8448)
)

# Crear el mapa base con las ciudades
map <- leaflet(ciudades_coord) %>%
  addTiles() %>%
  setView(lng = 105.8542, lat = 21.0285, zoom = 6) %>%
  addMarkers(
    lat = ~lat,
    lng = ~lon,
    label = ~name
  )

# Añadir las aristas del grafo al mapa
for (edge in E(g)) {
  from <- V(g)[ends(g, edge)[1]]$name
  to <- V(g)[ends(g, edge)[2]]$name
  from_coord <- ciudades_coord[ciudades_coord$name == from, c("lat", "lon")]
  to_coord <- ciudades_coord[ciudades_coord$name == to, c("lat", "lon")]
  
  map <- map %>% addPolylines(
    lng = c(from_coord$lon, to_coord$lon),
    lat = c(from_coord$lat, to_coord$lat),
    weight = 2,
    color = "blue",
    opacity = 0.5,
    label = paste("Distancia:", E(g)$weight[which(E(g) == edge)], "km")
  )
}

# Añadir el camino Hamiltoniano de menor distancia al mapa
for (i in 1:(length(min_distance_path) - 1)) {
  from <- V(g)[min_distance_path[i]]$name
  to <- V(g)[min_distance_path[i+1]]$name
  from_coord <- ciudades_coord[ciudades_coord$name == from, c("lat", "lon")]
  to_coord <- ciudades_coord[ciudades_coord$name == to, c("lat", "lon")]
  
  map <- map %>% addPolylines(
    lng = c(from_coord$lon, to_coord$lon),
    lat = c(from_coord$lat, to_coord$lat),
    weight = 4,
    color = "red",
    opacity = 0.8,
    label = paste("Camino Hamiltoniano: ", from, " -> ", to, " Distancia:", E(g)$weight[get.edge.ids(g, c(min_distance_path[i], min_distance_path[i+1]))], "km")
  )
}

# Mostrar el mapa
map
