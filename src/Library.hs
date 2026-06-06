module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | 
    Papas | PatiVegano | PanIntegral | BaconDeTofu
    deriving (Eq, Show)

data Hamburguesa = Hamburguesa {
    precioBase    :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

-- Parte 1

precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo = 10
precioIngrediente Curry = 5
precioIngrediente Papas = 10
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3
precioIngrediente BaconDeTofu = 12

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa =
    precioBase hamburguesa + (sum . map precioIngrediente . ingredientes) hamburguesa

getIngredienteBase :: Hamburguesa -> Ingrediente
getIngredienteBase = head . filter esIngredienteBase . ingredientes

esIngredienteBase :: Ingrediente -> Bool
esIngredienteBase ingrediente = elem ingrediente [PatiVegano, Carne, Pollo]

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa = agregarIngrediente (getIngredienteBase hamburguesa) hamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente nuevoIngrediente hamburguesa =
    hamburguesa { ingredientes = head (ingredientes hamburguesa) 
                               : nuevoIngrediente 
                               : tail (ingredientes hamburguesa) }

porcentaje :: Number -> Number
porcentaje unNumero = (100 - unNumero) / 100

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentajeDescuento hamburguesa =
    hamburguesa { precioBase = precioBase hamburguesa * porcentaje porcentajeDescuento }

-- Parte 2

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Cheddar . agregarIngrediente Carne $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra

delDia :: Hamburguesa -> Hamburguesa
delDia = descuento 30 . agregarIngrediente Papas

-- Parte 3

cambiarIngredienteAVegano :: Ingrediente -> Ingrediente
cambiarIngredienteAVegano Carne = PatiVegano
cambiarIngredienteAVegano Pollo = PatiVegano
cambiarIngredienteAVegano Cheddar = QuesoDeAlmendras
cambiarIngredienteAVegano Panceta = BaconDeTofu
cambiarIngredienteAVegano ingrediente = ingrediente

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa =
    hamburguesa { ingredientes = map cambiarIngredienteAVegano (ingredientes hamburguesa), 
                  precioBase = 10 }

cambiarPan :: Ingrediente -> Ingrediente
cambiarPan Pan = PanIntegral
cambiarPan ingrediente = ingrediente

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa =
    hamburguesa { ingredientes = map cambiarPan (ingredientes hamburguesa) }

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePati . hacerVeggie $ dobleCuarto