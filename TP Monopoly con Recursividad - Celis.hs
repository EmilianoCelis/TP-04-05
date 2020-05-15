module Main where

import Text.Show.Functions()

main :: IO ()
main = return ()

data Jugador = UnJugador {
    nombreDelJugador :: String,
    dineroDelJugador :: Float,
    tacticaDeJuego :: String,
    propiedades :: Propiedades,
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

modificarNombre :: String -> Jugador -> Jugador
modificarNombre prefijoParaElNombre unJugador = unJugador {
    nombreDelJugador = prefijoParaElNombre ++ nombreDelJugador unJugador
}

sumarDinero :: Float -> Jugador -> Jugador
sumarDinero unDinero unJugador = unJugador {
    dineroDelJugador = dineroDelJugador unJugador + unDinero
}

asignarTactica :: String -> Jugador -> Jugador
asignarTactica unaTactica unJugador = unJugador {
    tacticaDeJuego = unaTactica
}

agregarUnaPropiedad :: Propiedad -> Jugador -> Jugador
agregarUnaPropiedad unaPropiedad unJugador = unJugador {
    propiedades = propiedades unJugador ++ [unaPropiedad]
}

agregarAcciones :: Acciones -> Jugador -> Jugador
agregarAcciones unasAcciones unJugador = unJugador {
    accionesALoLargoDelJuego = accionesALoLargoDelJuego unJugador ++ unasAcciones
}

pasarPorElBanco :: Jugador -> Jugador
pasarPorElBanco unJugador = (sumarDinero 40 . asignarTactica "Comprador Complsivo" . agregarAcciones [pasarPorElBanco]) unJugador

enojarse :: Jugador -> Jugador
enojarse unJugador = (sumarDinero 50 . agregarAcciones [enojarse, gritar]) unJugador

gritar :: Jugador -> Jugador
gritar unJugador = (modificarNombre "AHHHH" . agregarAcciones [gritar]) unJugador

esCapazDeComprarLaPropiedad :: Jugador -> Float -> Bool
esCapazDeComprarLaPropiedad unJugador valor = dineroDelJugador unJugador >= valor

esDignoDePropiedad :: Jugador -> Propiedad -> Bool
esDignoDePropiedad unJugador unaPropiedad = elem (tacticaDeJuego unJugador) ["Oferente singular", "Accionista"] && esCapazDeComprarLaPropiedad unJugador (valorDeLaPropiedad unaPropiedad)

subastar :: Propiedad -> Jugador -> Jugador
subastar unaPropiedad unJugador 
    | esDignoDePropiedad unJugador unaPropiedad = (sumarDinero (-valorDeLaPropiedad unaPropiedad) . agregarUnaPropiedad unaPropiedad . agregarAcciones [subastar unaPropiedad]) unJugador
    | otherwise                                 = unJugador

cobrarUnAlquiler :: Propiedad -> Float
cobrarUnAlquiler unaPropiedad 
    | valorDeLaPropiedad unaPropiedad < 150 = 10
    | otherwise                             = 20

sumarAlquileres :: Propiedades -> Float
sumarAlquileres lasPropiedadesDelJugador = sum (map cobrarUnAlquiler lasPropiedadesDelJugador)

cobrarAlquileres :: Jugador -> Jugador
cobrarAlquileres (UnJugador unNombre dinero unaTactica [] unasAcciones) = UnJugador unNombre dinero unaTactica [] unasAcciones
cobrarAlquileres unJugador                                              = (sumarDinero (sumarAlquileres (propiedades unJugador)) . agregarAcciones [cobrarAlquileres]) unJugador

pagarAAccionistas :: Jugador -> Jugador
pagarAAccionistas (UnJugador unNombre dinero "Accionista" unasPropiedades unasAcciones) = UnJugador unNombre (dinero - 200) "Accionista" unasPropiedades (unasAcciones ++ [pagarAAccionistas])
pagarAAccionistas unJugador                                                             = (sumarDinero (-100) . agregarAcciones [pagarAAccionistas]) unJugador

hacerBerrinchePor :: Propiedad -> Jugador -> Jugador
hacerBerrinchePor unaPropiedad unJugador 
    | valorDeLaPropiedad unaPropiedad <= dineroDelJugador unJugador = (sumarDinero (-valorDeLaPropiedad unaPropiedad) . agregarUnaPropiedad unaPropiedad) unJugador
    | otherwise                                                     = hacerBerrinchePor unaPropiedad (gritar ((sumarDinero 10 . agregarAcciones [hacerBerrinchePor unaPropiedad]) unJugador))

aplicarAcciones :: Jugador -> Acciones -> Jugador
aplicarAcciones unJugador []           = unJugador
aplicarAcciones unJugador unasAcciones = aplicarAcciones (head unasAcciones unJugador) (tail unasAcciones)

ultimaRonda :: Jugador -> Jugador
ultimaRonda unJugador = aplicarAcciones unJugador (accionesALoLargoDelJuego unJugador)

juegoFinal :: Jugador -> Jugador -> Jugador
juegoFinal unJugador otroJugador
    | dineroDelJugador (ultimaRonda unJugador) > dineroDelJugador (ultimaRonda otroJugador) = ultimaRonda unJugador
    | otherwise                                                                             = ultimaRonda otroJugador
