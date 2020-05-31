import.Text.Show.Functions ()

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

-- Auxiliares

anio :: Fecha -> Int
anio (_,_,year) = year

segundoDeLaLista :: String -> Char
segundoDeLaLista lista = lista !! 1

-- Auxiliares

-- 1)

costoDeReparacion :: Auto -> Int
costoDeReparacion unAuto 
 | patenteTieneSieteDigitos (patente unAuto) = 12500 
 | patenteEstaEntreDJyNB (patente unAuto) && patenteTerminaEn4 (patente unAuto)  = 3 * (length (patente unAuto))
 | patenteEstaEntreDJyNB (patente unAuto)  = 20000
 | otherwise = 15000

patenteTieneSieteDigitos :: Patente -> Bool
patenteTieneSieteDigitos unaPatente = (== 7).length $ unaPatente 

patenteTerminaEn4 :: Patente -> Bool
patenteTerminaEn4 unaPatente = (==4). drop ((length unaPatente) - 1)  $ unaPatente 

patenteEstaEntreDJyNB :: Patente -> Bool
patenteEstaEntreDJyNB unaPatente = estaEntreDyN unaPatente && estaEntreJyB unaPatente

entreDyN :: Char -> Bool
entreDyN unaLetra = elem unaLetra ['D'..'N']

entreJyB :: Char -> Bool
entreJyB unaLetra = elem unaLetra ['J'..'B']

estaEntreDyN :: Patente -> Bool
estaEntreDyN = entreDyN.head 

estaEntreJyB :: Patente -> Bool
estaEntreJyB = entreJyB.segundoDeLaLista.take 2

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
bravo unAuto = unAuto {desgasteDeLlantas = [0.0,0.0,0.0,0.0]}

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
elementosImpares [] = []
elementosImpares [elemento] = filter odd [elemento]
elementosImpares [primero,segundo] = filter odd [primero,segundo]
elementosImpares (primero:_:cola) = (primero:elementosImpares(cola))

elementosPares :: [a] -> [a]
elementosPares [] = []
elementosPares [elemento] = filter even [elemento]
elementosPares [primero,segundo] = filter even [primero,segundo]
elementosPares (_:segundo:cola) = (segundo:elementosPares(cola))

desgasteDelAuto :: Auto -> Int
desgasteDelAuto unAuto = round.(10 *).sum.desgasteDeLlantas $ unAuto

-- 5)

ordenDeReparacion :: Fecha -> [Tecnico] -> TallerMecanico
ordenDeReparacion unaFecha unosTecnicos unAuto = (actualizarUltimaFechaDeReparacion unaFecha.realizarLasReparaciones unosTecnicos) unAuto

actualizarUltimaFechaDeReparacion :: Fecha -> Auto -> Auto
actualizarUltimaFechaDeReparacion fechaDelUltimoArreglo unAuto = unAuto {ultimoArreglo = fechaDelUltimoArreglo}

realizarLasReparaciones :: [Tecnico] -> TallerMecanico
realizarLasReparaciones unosTecnicos unAuto = foldl1 (.) unosTecnicos $ unAuto

-- 6)
-- Integrante a

{-
tecnicosQueDejanElAutoEnCondiciones ::  Auto -> [Tecnico] -> [Tecnico]
tecnicosQueDejanElAutoEnCondiciones unAuto unosTecnicos = filter (dejaElAutoEnCondiciones unosTecnicos unAuto) unosTecnicos

tecnicosQueDejanElAutoEnCondiciones' ::  Auto -> [Tecnico] -> [Auto]
tecnicosQueDejanElAutoEnCondiciones' unAuto unosTecnicos = filter autoEnCondiciones (arreglarAutos unosTecnicos unAuto) 
                 -- Esto funciona v ^
arreglarAutos :: [Tecnico] -> Auto -> [Auto]
arreglarAutos unosTecnicos unAuto = map ($unAuto) unosTecnicos


autoEnCondiciones :: Auto -> Bool
autoEnCondiciones = not.esUnAutoPeligroso 
-}

tecnicosQueDejanElAutoEnCondiciones :: [Tecnico] -> Auto ->  [Tecnico] 
tecnicosQueDejanElAutoEnCondiciones unosTecnicos unAuto = filter (arreglarAuto unAuto) unosTecnicos

arreglarAuto :: Auto -> Tecnico -> Bool
arreglarAuto unAuto unTecnico = (esUnAutoPeligroso.unTecnico)  unAuto 

-- Integrante b

costoDeReparacionDeAutosQueNecesitanRevision :: [Auto] -> [Int]
costoDeReparacionDeAutosQueNecesitanRevision unosAutos = map costoDeReparacion.autosQueNecesitanRevision $ unosAutos

autosQueNecesitanRevision :: [Auto] -> [Auto]
autosQueNecesitanRevision unosAutos = filter necesitaRevision unosAutos

--7)

--Integrante a

-- Si se puede hacer con una lista infinita ya que la funcion head trabaja a partir de una "evaluacion perezosa"
-- o lazy evaluation, entonces va a evaluar solo lo que necesita y despues tomar esos parametros.
-- Ej.
primerTecnicoEnCondiciones :: [Tecnico] -> Auto -> Tecnico
primerTecnicoEnCondiciones unosTecnicos unAuto = head (tecnicosQueDejanElAutoEnCondiciones unosTecnicos unAuto)

--Integrante b

-- No se puede hacer con una lista infinita porque filter trabaja con call-by-value y antes de aplicar la funcion
-- va a tener que cargar los parametros, lo cual, al ser infinitos, es imposible.
