# Usamos la imagen oficial de Haskell
FROM haskell:9.4

# Creamos el directorio de trabajo
WORKDIR /app

# Copiamos todos los .hs (Set_1, Set_2, etc.) al contenedor
COPY . /app

# Compilamos el ejecutable
RUN ghc -o mi_portfolio main.hs function_sets/set_1.hs

# Al arrancar el contenedor, ejecutamos el programa
CMD ["./mi_portfolio"]