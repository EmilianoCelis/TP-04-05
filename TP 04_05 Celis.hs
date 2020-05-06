module Main where

import Text.Show.Functions()

main :: IO ()
main = return ()

data Jugador = UnJugador {
    nombreDelJugador :: String,
    dineroDelJugador :: Float,
    tacticaDeJuego :: String,
    cantidadDePropiedades :: Propiedades,
    accionesALoLargoDelJuego :: Acciones
} deriving Show

data Propiedad = UnaPropiedad {
    nombreDeLaPropiedad :: String,
    valorDeLaPropiedad :: Float
} deriving (Show, Eq)

type Propiedades = [Propiedad]

type Acciones = [Jugador -> Jugador]

carolina :: Jugador
carolina = UnJugador "Carolina" 500 "Accionista" [] [pasarPorElBanco]

manuel :: Jugador
manuel = UnJugador "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse]

pasarPorElBanco :: Jugador -> Jugador
pasarPorElBanco unJugador = unJugador {
    dineroDelJugador = ((+40).dineroDelJugador) unJugador, 
    tacticaDeJuego = "Comprador Complusivo", 
    accionesALoLargoDelJuego = ((++[pasarPorElBanco]).accionesALoLargoDelJuego) unJugador
}

enojarse :: Jugador -> Jugador
enojarse unJugador = unJugador {
    dineroDelJugador = ((+50).dineroDelJugador) unJugador, 
    accionesALoLargoDelJuego = ((++[enojarse, gritar]).accionesALoLargoDelJuego) unJugador
}

gritar :: Jugador -> Jugador
gritar unJugador = unJugador {
    nombreDelJugador = ((++"AHHHH").nombreDelJugador) unJugador,  
    accionesALoLargoDelJuego = ((++[gritar]).accionesALoLargoDelJuego) unJugador
}

esCapazDeComprarLaPropiedad :: Jugador -> Float -> Bool
esCapazDeComprarLaPropiedad unJugador valor = dineroDelJugador unJugador >= valor

esDignoDePropiedad :: Jugador -> Propiedad -> Bool
esDignoDePropiedad unJugador unaPropiedad = elem (tacticaDeJuego unJugador) ["Oferente singular", "Accionista"] && esCapazDeComprarLaPropiedad unJugador (valorDeLaPropiedad unaPropiedad)

subastar :: Propiedad -> Jugador -> Jugador
subastar unaPropiedad unJugador 
    | esDignoDePropiedad unJugador unaPropiedad = unJugador { 
    dineroDelJugador = dineroDelJugador unJugador - valorDeLaPropiedad unaPropiedad,
    cantidadDePropiedades = (cantidadDePropiedades unJugador) ++ [unaPropiedad],
    accionesALoLargoDelJuego = ((++[subastar unaPropiedad]).accionesALoLargoDelJuego) unJugador
}   
    | otherwise = unJugador

cobrarUno :: Propiedad -> Float
cobrarUno unaPropiedad 
    | valorDeLaPropiedad unaPropiedad < 150 = 10
    | otherwise = 20

totalDelCobro :: Propiedades -> Float
totalDelCobro lasPropiedadesDelJugador = sum (map cobrarUno lasPropiedadesDelJugador)

cobrarAlquileres :: Jugador -> Jugador
cobrarAlquileres unJugador
    | cantidadDePropiedades unJugador == [] = unJugador
    | otherwise = unJugador {
    dineroDelJugador = dineroDelJugador unJugador + totalDelCobro (cantidadDePropiedades unJugador),
    accionesALoLargoDelJuego = ((++[cobrarAlquileres]).accionesALoLargoDelJuego) unJugador
}

pagarAAccionistas :: Jugador -> Jugador
pagarAAccionistas unJugador 
    | (tacticaDeJuego unJugador) == "Accionista" = unJugador {
    dineroDelJugador = dineroDelJugador unJugador - 200,
    accionesALoLargoDelJuego = ((++[pagarAAccionistas]).accionesALoLargoDelJuego) unJugador
}
    | otherwise = unJugador {
    dineroDelJugador = dineroDelJugador unJugador - 100,
    accionesALoLargoDelJuego = ((++[pagarAAccionistas]).accionesALoLargoDelJuego) unJugador
}