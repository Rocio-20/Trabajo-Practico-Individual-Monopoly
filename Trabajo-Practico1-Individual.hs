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

--Personas
carolina :: Persona
carolina = Persona{
    nombre = "Carolina",
    cantidadDeDinero = 500,
    tacticaDeJuego = "Accionista",
    propiedadesCompradas = [],
    acciones = [pasarPorElBanco, pagarAAcciones]
}

manuel :: Persona
manuel = Persona{
    nombre = "Manuel",
    cantidadDeDinero = 500,
    tacticaDeJuego = "Oferente singular",
    propiedadesCompradas = [],
    acciones = [pasarPorElBanco, enojarse]
}

--Acciones
pasarPorElBanco :: Accion
pasarPorElBanco = (modificarDinero 40).(cambiarTactica "CompradorCompulsivo")

modificarDinero :: Int -> Persona -> Persona
modificarDinero dinero persona = persona{cantidadDeDinero = cantidadDeDinero persona + dinero}

cambiarTactica :: String -> Persona -> Persona
cambiarTactica nuevaTactica persona = persona{tacticaDeJuego = nuevaTactica}


enojarse :: Accion
enojarse = (modificarDinero 50).(agregarAccion gritar)

agregarAccion :: Accion -> Persona -> Persona
agregarAccion accion persona = persona{acciones = accion : (acciones persona) }


gritar :: Accion
gritar persona= persona{nombre = "AHHHH" ++ nombre persona}


subastar :: Persona -> Propiedad -> Persona
subastar persona propiedad
 | puedeComprar persona propiedad= (modificarDinero (- precio propiedad)).(agregarPropiedad propiedad) $ persona 
 | otherwise = persona

agregarPropiedad :: Propiedad -> Persona -> Persona
agregarPropiedad propiedad persona = persona{propiedadesCompradas = propiedad : propiedadesCompradas persona}

puedeComprar :: Persona -> Propiedad -> Bool
puedeComprar persona propiedad= ((tieneTactica "OferenteSingular" persona) || (tieneTactica "Accionista" persona)) && cuentaConCapitalParaComprar persona propiedad

tieneTactica :: String -> Persona -> Bool
tieneTactica tactica = (== tactica).tacticaDeJuego

cuentaConCapitalParaComprar :: Persona -> Propiedad -> Bool
cuentaConCapitalParaComprar persona propiedad = precio propiedad <= cantidadDeDinero persona


cobrarAlquiler :: Accion
cobrarAlquiler persona = modificarDinero (- calculoDeAlquiler persona) persona

calculoDeAlquiler :: Persona -> Int
calculoDeAlquiler persona = 10*(cantidadDePropiedadBaratas persona) + 20*(cantidadDePropiedadCaras persona)

cantidadDePropiedadCaras :: Persona -> Int
cantidadDePropiedadCaras = length.(filter (not.propiedadBarata)).propiedadesCompradas

cantidadDePropiedadBaratas :: Persona -> Int
cantidadDePropiedadBaratas= length.filter propiedadBarata.propiedadesCompradas

propiedadBarata :: Propiedad -> Bool
propiedadBarata = (< 150).precio


pagarAAcciones :: Accion
pagarAAcciones persona 
  | not.(tieneTactica "Accionista") $ persona = modificarDinero (-100) persona
  | otherwise = modificarDinero 200 persona


hacerBerrinchePor :: Persona -> Propiedad -> Persona
hacerBerrinchePor persona propiedad 
  | cuentaConCapitalParaComprar persona propiedad = subastar persona propiedad
  | otherwise = hacerBerrinchePor (gritar.modificarDinero 10 $ persona) propiedad


juegoFinal :: Persona -> Persona -> Persona
juegoFinal participante1 participante2 
 | cantidadDeDinero participante1 > cantidadDeDinero participante2 = participante1
 | otherwise = participante2


ultimaRonda :: Persona -> Accion
ultimaRonda persona = foldl1 (.) (acciones persona)

