type Patente = String
type Desgaste = Float
type Fecha = (Int, Int, Int)
type TallerMecanico = Auto -> Auto
type Tecnico = TallerMecanico



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

-- 1)
-- Hay que usar composicion

costoDeReparacion :: Patente -> Auto -> Int
costoDeReparacion unaPatente unAuto 
 | patenteTieneSieteDigitos unaPatente = 12500 
 | patenteEstaEntreDJyNB unaPatente && patenteTerminaEn4 unaPatente  = 3 * (length unaPatente)
 | patenteEstaEntreDJyNB unaPatente  = 20000
 -- logicamente habria que pasarles un auto, porque es la patente de ESE auto pero hay q ver como hacer las funciones para que RECIBAn un auto y no tiren ERROOOOORRRRR
 | otherwise = 15000

patenteTieneSieteDigitos :: Patente -> Bool
patenteTieneSieteDigitos unaPatente = length unaPatente == 7


{-
patenteTieneSieteDigitos :: Patente -> Bool
patenteTieneSieteDigitos unaPatente = (== 7).length unaPatente 
-}

patenteEstaEntreDJyNB :: Patente -> Bool
patenteEstaEntreDJyNB unaPatente = estaEntreDyN (head unaPatente) && estaEntreJyB (tail (take 2 unaPatente))

-- lo de tail no lo entendi, se supone que son las patentes viejas

estaEntreDyN :: String -> Bool
estaEntreDyN unaLetra = elem unaLetra entreDyN

estaEntreJyB :: String -> Bool
estaEntreJyB unaLetra = elem unaLetra entreJyB

entreDyN :: [Char]
entreDyN = ['D'..'N']

entreJyB :: [Char]
entreJyB = ['J'..'B']

-- yo lo haria asi

{- 
estaEntreDyN' :: Char -> Bool
estaEntreDyN' unaLetra = elem unaLetra ['D'..'N']

estaEntreJyB' :: Char -> Bool
estaEntreJyB' unaLetra = elem unaLetra ['J'..'B']
-}

-- creo que hay algo que no termino de entender, la patente deberia estar entre DJ y NB, por que haces entre DN y JB
-- ponele DGO-234 esta en el rango porque el head esta entre D-J
-- pero en ese caso por que dicen "deberia estar entre xx Y entre xx"
-- no tiene SENTIDOOOOOOOOOOOOOOOOOOOOOO

patenteTerminaEn4 :: Patente -> Bool
patenteTerminaEn4 unaPatente =  drop ((length unaPatente) - 1) unaPatente == 4

-- 2)
-- Estas funciones deberian recibir un AUTO 
-- Integrante a

desgasteDeLaPrimerLlanta' :: [Desgaste] -> Float
desgasteDeLaPrimerLlanta' lasLlantas = head lasLlantas 

esUnAutoPeligroso' :: [Desgaste] -> Bool
esUnAutoPeligroso' lasLlantas = desgasteDeLaPrimerLlanta' lasLlantas > 0.5

-- opcion 2 para usar en pto 6 
esUnAutoPeligroso :: Auto -> Bool
esUnAutoPeligroso unAuto = desgasteDeLaPrimerLlanta unAuto > 0.5

desgasteDeLaPrimerLlanta :: Auto -> Auto
desgasteDeLaPrimerLlanta unAuto = unAuto {desgasteDeLlantas = head (desgasteDeLlantas unAuto)}

-- Integrante b

necesitaRevision :: Fecha -> Bool
necesitaRevision unaFecha  = anio unaFecha  <= 2015

-- opcion 2 que recibe un auto y tiene mas logica / para usar en pto 6
necesitaRevision' :: Auto -> Bool
necesitaRevision' unAuto = anioDelUltimoArreglo unAuto <= 2015

anioDelUltimoArreglo :: Auto -> Auto
anioDelUltimoArreglo unAuto = unAuto {ultimoArreglo = anio (ultimoArreglo unAuto)} 


-- 3) Taller Mecanico
-- Integrante a

alfa :: TallerMecanico
alfa unAuto
 | cumpleCondicionParaRegularLasVueltas unAuto = regularLasVueltas 2000 unAuto
 | otherwise = unAuto

cumpleCondicionParaRegularLasVueltas :: Auto -> Bool
cumpleCondicionParaRegularLasVueltas unAuto = rpm unAuto > 2000

regularLasVueltas :: Int -> TallerMecanico
regularLasVueltas revoluciones unAuto = unAuto {rpm = revoluciones} 

bravo :: Tecnico
bravo unAuto = unAuto {desgasteDeLlantas = [0]}  -- o seria = 0 nada mas?

charly :: Tecnico
charly = alfa bravo 

-- Integrante b

tango :: Tecnico
tango = id

zulu :: Tecnico
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

ordenDeReparacion :: Fecha -> [Tecnico] -> TallerMecanico 
ordenDeReparacion fecha unosTecnicos unAuto = unAuto {ultimoArreglo unAuto = fecha } && map ($unAuto) unosTecnicos

-- yo lo haria asi 

ordenDeReparacion' :: Fecha -> [Tecnico] -> TallerMecanico
ordenDeReparacion' unaFecha unosTecnicos unAuto = (actualizarUltimaFechaDeReparacion unaFecha.realizarLasReparaciones unosTecnicos) unAuto

actualizarUltimaFechaDeReparacion :: Fecha -> Auto -> Auto
actualizarUltimaFechaDeReparacion fechaDelUltimoArreglo unAuto = unAuto {ultimoArreglo = fechaDelUltimoArreglo}

-- tiene que recibir un auto SOLO, algo esta mal definido en la funcion
realizarLasReparaciones :: [Tecnico] -> TallerMecanico
realizarLasReparaciones unosTecnicos unAuto = map ($unAuto) unosTecnicos


-- 6)
-- Integrante a 

tecnicosQueDejanElAutoEnCondiciones :: [Tecnico] -> [Tecnico]
tecnicosQueDejanElAutoEnCondiciones unosTecnicos = filter autoEnCondiciones unosTecnicos

--tecnicosQueDejanElAutoEnCondiciones unosTecnicos unAuto = filter autoEnCondiciones (map ($unAuto) unosTecnicos)

autoEnCondiciones :: Auto -> Bool
autoEnCondiciones = not esUnAutoPeligroso 

-- Integrante b

costoDeReparacionDeAutosQueNecesitanRevision :: [Patente] -> [Auto] -> Int
costoDeReparacionDeAutosQueNecesitanRevision unasPatentes unosAutos = costoDeReparacion unasPatentes.autosQueNecesitanRevision unosAutos  

autosQueNecesitanRevision :: [Auto] -> [Auto]
autosQueNecesitanRevision unosAutos = filter necesitaRevision' unosAutos

--7)

--Integrante a

primerTecnicoEnCondiciones unosTecnicos unAuto = head (tecnicosQueDejanElAutoEnCondiciones unosTecnicos unAuto)

--Integrante b

-- No se puede hacer con una lista infinita


