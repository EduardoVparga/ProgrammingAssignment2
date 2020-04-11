## La funciones permiten al usuario crear una matriz cuadrada que por defecto tendra valores aleatorios
## entre 1 y 1000 con una dimension de 2x2 y calcular, capturar y/o remplazar la matriz inversa

## Esta funcion permite crear una matriz con valores aleatorios entre el rango que el usuario desee y con
## las dimesiones que prefiera, llamarla, calcular y obtener la matriz inversa o asignarla según sea el
## caso. 

makeCacheMatrix <- function(x = matrix()) {
  matriz_inv = NULL
  
  create_matrix = function(range_val = 1:1000, nr_nc = 2){
    matriz <<- matrix(sample(range_val, nr_nc*nr_nc, replace = T), nr_nc, nr_nc)}
  
  get_matriz = function() matriz
  cal_inv = function(matriz) matriz_inv <<- solve(matriz)
  get_m_inv = function() matriz_inv
  set_m_inv = function(matriz_inv_) matriz_inv <<- matriz_inv_ 
  
  list(create_matrix = create_matrix,
       cal_inv = cal_inv,
       get_m_inv = get_m_inv,
       get_matriz = get_matriz,
       set_m_inv = set_m_inv)

}


## Esta función permite capturar de manera aislada la matriz inversa del resultado de la función anterior
## Si el usuario no la ha calculado anteriormente, la función la calculará automaticamente.

cacheSolve <- function(matriz, ...) {
  matriz_cach_inv = matriz$matriz_inv
  if(!is.null(matriz_cach_inv)){
    return(matriz_cach_inv)}
  
  matriz_data = matriz$get_matriz()
  matriz_cach_inv = matriz$cal_inv(matriz_data)
  matriz$set_m_inv(matriz_cach_inv)
  matriz$get_m_inv()
}
