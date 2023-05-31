module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Sabor = String
type Peso = Number
type Temperatura = Number


data Postre = UnPostre {
    sabores :: [Sabor],
    peso :: Peso,
    temperatura :: Temperatura

}deriving (Show, Eq)

bizcocho :: Postre
bizcocho = UnPostre{
    sabores = ["borracho", "fruta", "crema"],
    peso = 100,
    temperatura = 25
}

tartaMelaza :: Postre
tartaMelaza = UnPostre{
    sabores = ["melaza"],
    peso = 50,
    temperatura = 0 
}

type Hechizo = Postre -> Postre

incendio:: Hechizo
incendio postre = postre { temperatura = (temperatura postre +1), peso = (peso postre - (peso postre*0.05) ) }


riddikulus :: Sabor -> Hechizo
riddikulus sabor postre = postre {sabores = reverse (sabor : sabores postre)}

inmobilus :: Hechizo 
inmobilus postre = postre {temperatura = 0} 

wingardiumLeviosa :: Hechizo
wingardiumLeviosa postre = postre {sabores = "concentrado" : sabores postre, peso = (peso postre * 0.90)}

diffindo::Number->Hechizo
diffindo porcentaje postre = postre {peso = (peso postre - ((porcentaje*peso postre)/100))} 

avadaKedavra:: Hechizo
avadaKedavra = (\postre ->postre {sabores = []}).inmobilus 

--vaciarSabores :: Hechizo
--vaciarSabores postre = postre {sabores = []}

estaListo::Postre->Bool
estaListo postre = (tienePeso postre)&&(tieneSabor postre)&&(noEstaCongelado postre)

estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo = all (estaListo.hechizo)

tienePeso :: Postre -> Bool
-- tienePeso postre = peso postre > 0
tienePeso = (>0).peso

tieneSabor :: Postre -> Bool
tieneSabor postre = not (null (sabores postre))

noEstaCongelado :: Postre -> Bool
-- estaCongelado postre = temperatura postre > 0
noEstaCongelado = (>0) . temperatura


-- sacarPromedio:: [Postre]->Number
-- sacarPromedio postres = (sum (map (peso postres)) / length postres)

promedio :: [Number] -> Number
promedio nums = sum nums / length nums 

pesoPromedioListos :: [Postre] -> Number
pesoPromedioListos = promedio . map peso . filter estaListo



data Mago = UnMago {
    hechizos :: [Hechizo],
    horrocruxes :: Number
} deriving (Show, Eq)


harryElSucioPotter :: Mago
harryElSucioPotter = UnMago {
    hechizos = [incendio, inmobilus, wingardiumLeviosa],
    horrocruxes = 3
}


practicarHechizo:: Hechizo->Mago->Mago
practicarHechizo hechizo mago= mago { hechizos = hechizo : hechizos mago  }


sumarHorrocrux :: Hechizo -> Postre -> Mago -> Mago
sumarHorrocrux hechizo postre mago
    | igualAAvadaKedavra hechizo postre = mago {horrocruxes = horrocruxes mago + 1}
    | otherwise = mago

igualAAvadaKedavra :: Hechizo -> Postre -> Bool
igualAAvadaKedavra hechizo postre = hechizo postre == avadaKedavra postre

irAClase :: Hechizo -> Postre -> Mago -> Mago
irAClase hechizo postre =  sumarHorrocrux hechizo postre . practicarHechizo hechizo

esMejor :: Postre -> Hechizo -> Hechizo -> Bool
esMejor postre hechizo1 hechizo2 = (length . sabores . hechizo1) postre > (length . sabores . hechizo2) postre

elMejorEntre :: Postre -> Hechizo -> Hechizo -> Hechizo
elMejorEntre postre hechizo1 hechizo2
    |esMejor postre hechizo1 hechizo2 = hechizo1
    |otherwise = hechizo2

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = foldl1 (elMejorEntre postre) (hechizos mago)

mesa :: [Postre]
mesa = repeat bizcocho

hechizoInfinito :: Mago
hechizoInfinito = UnMago {
    hechizos = repeat incendio,
    horrocruxes = 802
}

