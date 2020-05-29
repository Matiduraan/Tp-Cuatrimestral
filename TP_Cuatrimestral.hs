type Patente = String
type Desgaste = Float
type Fecha = (Int, Int, Int)
type TallerMecanico = Auto -> Auto
type Tecnicos = TallerMecanico



data Auto = UnAuto { patente :: Patente,
                     desgasteDeLlantas :: [Desgaste],
                     rpm :: Int,
                     temperaturaAgua :: Int,
                     ultimoArreglo :: Fecha
                   } deriving (Show)

{-
 El auto tiene: 
-la patente, que puede tener formato viejo “RVM363” o el nuevo “AB808RD”
-el desgaste de cada una de las llantas, ej: [ 0.5, 0.1, 0, 0.2 ]
-las revoluciones por minuto a las que regula el motor, ej: 1500
-la temperatura del agua luego de 5 minutos de encendido el auto: 90
-la fecha del último arreglo
-}

anio :: Fecha -> Int
anio (_,_,year) = year


costoDeReparacion :: Patente -> Auto -> Int
costoDeReparacion unaPatente unAuto 
 | patenteTieneSieteDigitos unaPatente unAuto = 12500
 | patenteEstaEntreDJyNB unaPatente unAuto && patenteTerminaEn4 unaPatente unAuto = 3 * (length unaPatente)
 | patenteEstaEntreDJyNB unaPatente unAuto = 20000
 | otherwise = 15000

patenteTieneSieteDigitos :: Patente -> Bool
patenteTieneSieteDigitos unaPatente = length unaPatente == 7

patenteEstaEntreDJyNB :: Patente -> Bool
patenteEstaEntreDJyNB unaPatente = estaEntreDyN (head unaPatente) && estaEntreJyB (tail (take 2 unaPatente))

-- tambien cambiaria lo que recibe estaEntreJyB y haria una funcion para devolver la cola de la lista para que no quede tan algoritmica (?

estaEntreDyN :: String -> Bool
estaEntreDyN unaLetra = elem unaLetra entreDyN

estaEntreJyB :: String -> Bool
estaEntreJyB unaLetra = elem unaLetra entreJyB

entreDyN :: [char]
entreDyN = ['D'..'N']

entreJyB :: [char]
entreJyB = ['J'..'B']

patenteTerminaEn4 :: Patente -> Bool
patenteTerminaEn4 unaPatente =  drop ((length unaPatente) - 1) unaPatente == 4

-- 2)
-- Estas funciones deberian recibir un AUTO 
-- Integrante a

desgasteDeLaPrimerLlanta :: [Desgaste] -> Float
desgasteDeLaPrimerLlanta lasLlantas = head lasLlantas 

esUnAutoPeligroso :: [Desgaste] -> Bool
esUnAutoPeligroso lasLlantas = desgasteDeLaPrimerLlanta lasLlantas > 0.5

-- opcion 2 para usar en pto 6 
esUnAutoPeligroso' :: Auto -> Bool
esUnAutoPeligroso' unAuto = desgasteDeLaPrimerLlanta' unAuto > 0.5

desgasteDeLaPrimerLlanta' :: Auto -> Auto
desgasteDeLaPrimerLlanta' unAuto = unAuto {desgasteDeLlantas = head desgasteDeLlantas unAuto}

-- Integrante b

necesitaRevision :: Fecha -> Bool
necesitaRevision unaFecha  = anio unaFecha  <= 2015

-- opcion 2 que recibe un auto y tiene mas logica / para usar en pto 6
necesitaRevision' :: Auto -> Bool
necesitaRevision' unAuto = anioDelUltimoArreglo unAuto <= 2015

anioDelUltimoArreglo :: Auto -> Fecha
anioDelUltimoArreglo unAuto = unAuto {ultimoArreglo = anio ultimoArreglo unAuto} -- POR QUE ESTA MAAAAAL LPM

-- 3) Taller Mecanico
-- Integrante a

alfa :: TallerMecanico
alfa unAuto
 | cumpleCondicionParaRegularLasVueltas unAuto = regularLasVueltas 2000 unAuto
 | otherwise = unAuto

regularLasVueltas :: Int -> TallerMecanico
regularLasVueltas revoluciones unAuto = unAuto {rpm = revoluciones} 

cumpleCondicionParaRegularLasVueltas :: Auto -> Bool
cumpleCondicionParaRegularLasVueltas unAuto = rpm unAuto > 2000

bravo :: TallerMecanico
bravo unAuto = unAuto {desgasteDeLlantas = 0}

charly :: TallerMecanico
charly = alfa.bravo 

-- Integrante b

tango :: TallerMecanico
tango unAuto = unAuto

zulu :: TallerMecanico
zulu = lima.cambiarTemperaturaANoventa 

cambiarTemperaturaANoventa :: TallerMecanico
cambiarTemperaturaANoventa unAuto = unAuto {temperaturaAgua = 90}

lima :: TallerMecanico
lima unAuto = unAuto {desgasteDeLlantas = cambiarDesgasteDePrimerasLlantas (desgasteDeLlantas unAuto)}

cambiarDesgasteDePrimerasLlantas :: [Desgaste] -> [Desgaste]
cambiarDesgasteDePrimerasLlantas lasLlantas = [0.0,0.0] ++ drop 2 lasLlantas

-- 4) Ordenamiento TOC de autos

ordenamientoTOC :: [Auto] -> Bool
ordenamientoTOC listaDeAutos = all odd (map desgasteDelAuto (elementosImpares listaDeAutos)) && all even (map desgasteDelAuto (elementosPares listaDeAutos))

elementosImpares :: [a] -> [a]
elementosImpares [elemento] = filter odd [elemento]
elementosImpares [primero,segundo] = filter odd [primero,segundo]
elementosImpares (primero:segundo:cola) = (primero:numerosImpares(cola))

elementosPares :: [a] -> [a]
elementosPares [elemento] = filter even [elemento]
elementosPares [primero,segundo] = filter even [primero,segundo]
elementosPares (primero:segundo:cola) = (segundo:numerosImpares(cola))



desgasteDelAuto :: Auto -> Int
desgasteDelAuto unAuto = round (10 * (sum (desgasteDeLlantas unAuto)))


-- Pto 5
-- aca no seria Fecha -> TallerMecanico -> Auto ?

ordenDeReparacion :: Fecha -> TallerMecanico -> TallerMecanico 
ordenDeReparacion fecha unosTecnicos unAuto = unAuto {ultimoArreglo unAuto = fecha } && map ($unAuto) unosTecnicos

-- 6)
-- Integrante a 

tecnicosQueDejanElAutoEnCondiciones :: [Tecnicos] -> [Tecnicos]
tecnicosQueDejanElAutoEnCondiciones unosTecnicos = filter autoEnCondiciones unosTecnicos

autoEnCondiciones :: Auto -> Bool
autoEnCondiciones = not esUnAutoPeligroso' 

-- Integrante b

costoDeReparacionDeAutosQueNecesitanRevision :: [Patente] -> [Auto] -> Int
costoDeReparacionDeAutosQueNecesitanRevision unasPatentes unosAutos = costoDeReparacion unasPatentes.autosQueNecesitanRevision unosAutos  

autosQueNecesitanRevision :: [Auto] -> [Auto]
autosQueNecesitanRevision unosAutos = filter necesitaRevision' unosAutos




