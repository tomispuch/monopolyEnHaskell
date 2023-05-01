import Text.Show.Functions ()
data Participante = UnParticipante {
    nombre      :: String,
    dinero      :: Int,
    tactica     :: String,
    propiedades :: [Propiedad],
    acciones    :: [Acciones]
}deriving Show

type Propiedad = (NombrePropiedad , PrecioPropiedad)
type Acciones = Participante -> Participante
type NombrePropiedad = String
type PrecioPropiedad = Int

carolina :: Participante
carolina = UnParticipante { 
    nombre = "Carolina", 
    dinero = 500, 
    tactica = "Accionista",  
    propiedades = [],
    acciones = []
    } 

manuel :: Participante
manuel = UnParticipante { 
    nombre = "Manuel", 
    dinero =500 , 
    tactica = "Oferente Singular",  
    propiedades = [] ,
    acciones = [pasarPorElBanco] } 


pasarPorElBanco :: Acciones
pasarPorElBanco = agregarPlata 40 . cambiarTactica "Comprador Compulsivo"

agregarPlata :: Int -> Participante -> Participante
agregarPlata unDinero unParticipante = unParticipante {dinero = dinero unParticipante + unDinero}

cambiarTactica :: String -> Participante -> Participante
cambiarTactica unaTactica unParticipante = unParticipante {tactica = unaTactica}

enojarse :: Acciones
enojarse = agregarPlata 50 . gritar


gritar :: Acciones
gritar unParticipante = unParticipante{nombre = nombre unParticipante ++ "AHHHH"}


subastar :: Propiedad -> Int ->Participante -> Participante
subastar propiedadAComprar precio unParticipante
    | puedeSubastar unParticipante = (sumarAdquisicion propiedadAComprar . quitarPlata precio) unParticipante
    | otherwise = unParticipante

sumarAdquisicion :: Propiedad -> Participante -> Participante
sumarAdquisicion propiedadNueva unParticipante = unParticipante { propiedades = propiedadNueva : propiedades unParticipante}

quitarPlata :: Int -> Participante -> Participante
quitarPlata unaCantidad alguienQueJuega = alguienQueJuega { dinero = dinero alguienQueJuega - unaCantidad }

puedeSubastar:: Participante -> Bool
puedeSubastar unParticipante = tactica unParticipante == "Accionista" || tactica unParticipante == "Oferente singular"


type ListaDePropiedades = [Propiedad]
type PrecioAlquiler = Int

cobrarAlquileres:: Acciones
cobrarAlquileres unJugador = unJugador{ dinero = dinero unJugador + conteoDePrecios (propiedades unJugador) }

conteoDePrecios :: ListaDePropiedades -> Int
conteoDePrecios  = sum . map barataOCara

barataOCara :: Propiedad -> PrecioAlquiler
barataOCara (_,precio)
    | precio >= 150 = 20
    | otherwise = 10

pagarAAccionistas :: Acciones
pagarAAccionistas unParticipante
    | esAccionista unParticipante = unParticipante {dinero = dinero unParticipante + 200}
    | otherwise = unParticipante {dinero = dinero unParticipante - 100}

esAccionista :: Participante -> Bool
esAccionista unParticipante = tactica unParticipante == "Accionista"