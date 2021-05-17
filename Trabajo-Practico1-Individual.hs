import Text.Show.Functions

data Persona = Persona{
   nombre :: String,
   cantidadDeDinero :: Int,
   tacticaDeJuego :: String,
   propiedadesCompradas :: [Propiedad],
   acciones :: [Accion]
}deriving Show

type Accion = Persona -> Persona

data Propiedad = Propiedad{
    nombrePropiedad :: String,
    precio :: Int
}deriving Show

--PERSONAS
carolina :: Persona
carolina = Persona{
    nombre = "Carolina",
    cantidadDeDinero = 500,
    tacticaDeJuego = "Accionista",
    propiedadesCompradas = [],
    acciones = []
}

manuel :: Persona
manuel = Persona{
    nombre = "Manuel",
    cantidadDeDinero = 500,
    tacticaDeJuego = "Oferente singular",
    propiedadesCompradas = [],
    acciones = [pasarPorElBanco, enojarse]
}

--ACCIONES
--Pasar por el banco
pasarPorElBanco :: Accion
pasarPorElBanco = (aumentarDinero 40).(cambiarTactica "CompradorCompulsivo")

aumentarDinero :: Int -> Persona -> Persona
aumentarDinero dinero persona = persona{cantidadDeDinero = cantidadDeDinero persona + dinero}

cambiarTactica :: String -> Persona -> Persona
cambiarTactica nuevaTactica persona = persona{tacticaDeJuego = nuevaTactica}

--Enojarse
enojarse :: Accion
enojarse = (aumentarDinero 50).(agregarAccion gritar)

agregarAccion :: Accion -> Persona -> Persona
agregarAccion accion persona = persona{acciones = accion : (acciones persona) }

--Gritar
gritar :: Accion
gritar persona= persona{nombre = "AHHHH" ++ nombre persona}

--Subastar
subastar :: Persona -> Propiedad -> Persona
subastar persona propiedad
 | puedeSubastar persona propiedad= (aumentarDinero (- precio propiedad)).(agregarPropiedad propiedad) $ persona 
 | otherwise = persona

agregarPropiedad :: Propiedad -> Persona -> Persona
agregarPropiedad propiedad persona = persona{propiedadesCompradas = propiedad : propiedadesCompradas persona}

puedeSubastar :: Persona -> Propiedad -> Bool
puedeSubastar persona propiedad= ((tieneTactica "OferenteSingular" persona) || (tieneTactica "Accionista" persona)) && cuentaConCapitalParaComprar persona propiedad

tieneTactica :: String -> Persona -> Bool
tieneTactica tactica = (== tactica).tacticaDeJuego

cuentaConCapitalParaComprar :: Persona -> Propiedad -> Bool
cuentaConCapitalParaComprar persona propiedad = precio propiedad <= cantidadDeDinero persona

--Cobrar alquiler
cobrarAlquiler :: Accion
cobrarAlquiler persona= aumentarDinero (- calculoDeAlquiler persona) persona

calculoDeAlquiler :: Persona -> Int
calculoDeAlquiler persona= sum.map costoAlquiler $ propiedadesCompradas persona

costoAlquiler :: Propiedad -> Int
costoAlquiler propiedad
 | propiedadBarata propiedad = 10
 | otherwise = 20

propiedadBarata :: Propiedad -> Bool
propiedadBarata = (< 150).precio

--Pagar a acciones
pagarAAcciones :: Accion
pagarAAcciones persona 
  | not.(tieneTactica "Accionista") $ persona = aumentarDinero (-100) persona
  | otherwise = aumentarDinero 200 persona

--Hacer berrinche
hacerBerrinchePor :: Persona -> Propiedad -> Persona
hacerBerrinchePor persona propiedad 
  | cuentaConCapitalParaComprar persona propiedad = subastar persona propiedad
  | otherwise = hacerBerrinchePor (gritar.aumentarDinero 10 $ persona) propiedad

--Juego final
juegoFinal :: Persona -> Persona -> Persona
juegoFinal participante1 participante2 
 | cantidadDeDinero participante1 > cantidadDeDinero participante2 = participante1
 | otherwise = participante2

--Ultima ronda
ultimaRonda :: Persona -> Accion
ultimaRonda persona 
 | null.acciones $ persona = id
 | otherwise = foldl1 (.) (acciones persona)