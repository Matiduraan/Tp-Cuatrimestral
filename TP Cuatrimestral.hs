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

-- Auxiliares

mapDesgaste :: ([Desgaste] -> [Desgaste]) -> Auto -> Auto
mapDesgaste unaFuncion unAuto = unAuto {desgasteDeLlantas = unaFuncion.desgasteDeLlantas $ unAuto}

mapUltimoArreglo :: (Fecha -> Fecha) -> Auto -> Auto
mapUltimoArreglo unaFuncion unAuto = unAuto {ultimoArreglo = unaFuncion.ultimoArreglo $ unAuto}

-- Auxiliares


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
patenteTieneSieteDigitos unaPatente = (== 7).length $ unaPatente 
-- Voy a tener que aprender a usar ese signo de inte
{-
patenteTieneSieteDigitos :: Patente -> Bool
patenteTieneSieteDigitos unaPatente = (== 7).length unaPatente 
-}

patenteEstaEntreDJyNB :: Patente -> Bool
patenteEstaEntreDJyNB unaPatente = estaEntreDyN (head unaPatente) && estaEntreJyB (tail (take 2 unaPatente))

estaEntreDyN :: Char -> Bool
estaEntreDyN unaLetra = elem unaLetra ['D'..'N']

estaEntreJyB :: Char -> Bool
estaEntreJyB unaLetra = elem unaLetra ['J'..'B']

patenteTerminaEn4 :: Patente -> Bool
patenteTerminaEn4 unaPatente =  drop ((length unaPatente) - 1) unaPatente == 4

-- 2)

-- Integrante a

esUnAutoPeligroso :: Auto -> Bool
esUnAutoPeligroso unAuto = (>0.5).desgasteDeLaPrimerLlanta $ unAuto 

desgasteDeLaPrimerLlanta :: Auto -> Float
desgasteDeLaPrimerLlanta = head.desgasteDeLlantas

-- Integrante b

necesitaRevision :: Auto -> Bool
necesitaRevision unAuto = anioDelUltimoArreglo unAuto <= 2015

anioDelUltimoArreglo :: Auto -> Int
anioDelUltimoArreglo  = anio.ultimoArreglo 

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
charly = alfa.bravo 

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


-- 5)
 
ordenDeReparacion :: Fecha -> [Tecnico] -> TallerMecanico
ordenDeReparacion unaFecha unosTecnicos unAuto = (actualizarUltimaFechaDeReparacion unaFecha.realizarLasReparaciones unosTecnicos) unAuto

actualizarUltimaFechaDeReparacion :: Fecha -> Auto -> Auto
actualizarUltimaFechaDeReparacion fechaDelUltimoArreglo unAuto = unAuto {ultimoArreglo = fechaDelUltimoArreglo}

realizarLasReparaciones :: [Tecnico] -> TallerMecanico
realizarLasReparaciones unosTecnicos unAuto = foldl1 (.) unosTecnicos $ unAuto

-- 6)
-- Integrante a

tecnicosQueDejanElAutoEnCondiciones ::  Auto -> [Tecnico] -> [Tecnico]
tecnicosQueDejanElAutoEnCondiciones unAuto unosTecnicos = filter (dejaElAutoEnCondiciones unosTecnicos) unAuto 

dejaElAutoEnCondiciones :: [Tecnico] -> Auto -> Bool
dejaElAutoEnCondiciones unosTecnicos unAuto = autoEnCondiciones.realizarLasReparaciones unosTecnicos $ unAuto


--tecnicosQueDejanElAutoEnCondiciones unosTecnicos unAuto = filter autoEnCondiciones (map ($unAuto) unosTecnicos)
-- ahora necesito pasarle a tecnicosQueDejanElAutoEnCondiciones esa funcion y una lista de tecnicos, para que me devuelva


autoEnCondiciones :: Auto -> Bool
autoEnCondiciones = not.esUnAutoPeligroso 

-- Integrante b

costoDeReparacionDeAutosQueNecesitanRevision :: [Patente] -> [Auto] -> Int
costoDeReparacionDeAutosQueNecesitanRevision unasPatentes unosAutos = costoDeReparacion unasPatentes.autosQueNecesitanRevision unosAutos  

autosQueNecesitanRevision :: [Auto] -> [Auto]
autosQueNecesitanRevision unosAutos = filter necesitaRevision unosAutos

--7)

--Integrante a

-- Si se puede hacer con una lista infinita ya que la funcion head trabaja a partir de una "evaluacion perezosa"
-- o lazy evaluation, entonces va a evaluar solo lo que necesita y despues tomar esos parametros.
-- Ej.
primerTecnicoEnCondiciones unosTecnicos unAuto = head (tecnicosQueDejanElAutoEnCondiciones unosTecnicos unAuto)

--Integrante b

-- No se puede hacer con una lista infinita porque filter trabaja con call-by-value y antes de aplicar la funcion
-- va a tener que cargar los parametros, lo cual, al ser infinitos, es imposible.