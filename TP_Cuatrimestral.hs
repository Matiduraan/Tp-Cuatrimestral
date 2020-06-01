import Text.Show.Functions ()

-- Definiciones base
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


-- Ejemplos de autos
chevrolet :: Auto
chevrolet = UnAuto "AT001LN" [0.5, 0.1, 0.6, 0.4] 2000 120 (10,12,2014)

ferrari :: Auto
ferrari = UnAuto "DJV214" [0.51, 0.1, 0.6, 0.4] 2001 200 (23,5,2018)

bugatti :: Auto
bugatti = UnAuto "DJV215" [0.2, 0.5, 0.6, 0.1] 1000 300 (12,7,2016)

shelby :: Auto
shelby = UnAuto "DFH029" [0.1,0.4, 0.2, 0.0] 2100 30 (8,10,2015)

nissan :: Auto
nissan = UnAuto "NBA154" [0.1,0.4, 0.2, 0.0] 3000 70 (5,5,2013)

audi :: Auto
audi = UnAuto "JN123EF" [0.3, 0.5, 0.6, 0.1] 2500 90 (31,12,2015)

kia :: Auto
kia = UnAuto "AA111BB" [0.1, 0.1, 0.1, 0.0] 1500 100 (18,9,2020)

toyota :: Auto
toyota = UnAuto "DJ020MP" [0.6, 0.2, 0.4, 0.8] 2001 30 (1,1,2010)

panther6 :: Auto
panther6 = UnAuto "SE156IS" [0.8,0.2,0.5,0.7,0.1,0.9] 3000 170 (16,1,1977)

-- Auxiliares

anio :: Fecha -> Int
anio (_,_,year) = year

segundoDeLaLista :: String -> Char
segundoDeLaLista lista = lista !! 1

cambiarDesgasteDeLlantas :: ([Desgaste] -> [Desgaste]) -> Auto -> Auto
cambiarDesgasteDeLlantas unaFuncion unAuto = unAuto {desgasteDeLlantas = unaFuncion.desgasteDeLlantas $ unAuto}

-- 1)

costoDeReparacion :: Auto -> Int
costoDeReparacion unAuto 
 | patenteTieneSieteDigitos (patente unAuto) = 12500 
 | patenteEstaEntreDJyNB (patente unAuto) && patenteTerminaEn4 (patente unAuto)  = 3000 * (length (patente unAuto))
 | patenteEstaEntreDJyNB (patente unAuto)  = 20000
 | otherwise = 15000

patenteTieneSieteDigitos :: Patente -> Bool
patenteTieneSieteDigitos unaPatente = (== 7).length $ unaPatente 

patenteTerminaEn4 :: Patente -> Bool
patenteTerminaEn4 unaPatente = esPatenteVieja unaPatente && last unaPatente == '4' 

esPatenteVieja :: Patente -> Bool
esPatenteVieja = not.patenteTieneSieteDigitos

patenteEstaEntreDJyNB :: Patente -> Bool
patenteEstaEntreDJyNB unaPatente = "DJ" < (take 2 unaPatente) && (take 2 unaPatente) < "NB"

-- 2)

-- Integrante a

esUnAutoPeligroso :: Auto -> Bool
esUnAutoPeligroso = (> 0.5).desgasteDeLaPrimerLlanta

desgasteDeLaPrimerLlanta :: Auto -> Float
desgasteDeLaPrimerLlanta = head.desgasteDeLlantas

-- Integrante b

necesitaRevision :: Auto -> Bool
necesitaRevision = (<= 2015).anioDelUltimoArreglo  

anioDelUltimoArreglo :: Auto -> Int
anioDelUltimoArreglo  = anio.ultimoArreglo 

-- 3) Taller Mecanico

-- Integrante a

alfa :: Tecnico
alfa unAuto
 | cumpleCondicionParaRegularLasVueltas unAuto = regularLasVueltas 2000 unAuto
 | otherwise = unAuto

cumpleCondicionParaRegularLasVueltas :: Auto -> Bool
cumpleCondicionParaRegularLasVueltas unAuto = rpm unAuto > 2000

regularLasVueltas :: Int -> TallerMecanico
regularLasVueltas revoluciones unAuto = unAuto {rpm = revoluciones} 

bravo :: Tecnico
bravo = cambiarDesgasteDeLlantas (dejarCubiertasSinDesgaste) 

dejarCubiertasSinDesgaste :: [Desgaste] -> [Desgaste]
dejarCubiertasSinDesgaste desgasteDeLasLlantas = map desgasteEnCero desgasteDeLasLlantas

desgasteEnCero :: Desgaste -> Desgaste
desgasteEnCero _ = 0

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
lima unAuto = unAuto {desgasteDeLlantas = cambiarDesgasteDePrimerasLlantas.desgasteDeLlantas $ unAuto}

cambiarDesgasteDePrimerasLlantas :: [Desgaste] -> [Desgaste]
cambiarDesgasteDePrimerasLlantas lasLlantas = [0.0,0.0] ++ drop 2 lasLlantas

-- 4) Ordenamiento TOC de autos

tienenOrdenamientoTOC :: [Auto] -> Bool
tienenOrdenamientoTOC listaDeAutos = autosUbicadosEnPosicionImparTienenDesgasteImpar listaDeAutos && autosUbicadosEnPosicionParTienenDesgastePar listaDeAutos

autosUbicadosEnPosicionImparTienenDesgasteImpar :: [Auto] -> Bool
autosUbicadosEnPosicionImparTienenDesgasteImpar = all odd.map desgasteDelAuto.elementosImpares 

autosUbicadosEnPosicionParTienenDesgastePar :: [Auto] -> Bool
autosUbicadosEnPosicionParTienenDesgastePar = all even.map desgasteDelAuto.elementosPares 

--cambiar
elementosImpares :: [a] -> [a]
elementosImpares [] = []
elementosImpares [elemento] = [elemento]
elementosImpares [primero,_] = [primero]
elementosImpares (primero:_:cola) = (primero:elementosImpares(cola))

elementosPares :: [a] -> [a]
elementosPares [] = []
elementosPares [_] = []
elementosPares [_,segundo] = [segundo]
elementosPares (_:segundo:cola) = (segundo:elementosPares(cola))

desgasteDelAuto :: Auto -> Int
desgasteDelAuto = round.(10 *).sum.desgasteDeLlantas

-- 5)

ordenDeReparacion :: Fecha -> [Tecnico] -> TallerMecanico
ordenDeReparacion unaFecha unosTecnicos = actualizarUltimaFechaDeReparacion unaFecha.realizarLasReparaciones unosTecnicos 

actualizarUltimaFechaDeReparacion :: Fecha -> Auto -> Auto
actualizarUltimaFechaDeReparacion fechaDelUltimoArreglo unAuto = unAuto {ultimoArreglo = fechaDelUltimoArreglo}

realizarLasReparaciones :: [Tecnico] -> TallerMecanico
realizarLasReparaciones unosTecnicos  = foldl1 (.) unosTecnicos  

-- 6)

-- Integrante a

tecnicosQueDejanElAutoEnCondiciones :: [Tecnico] -> Auto ->  [Tecnico] 
tecnicosQueDejanElAutoEnCondiciones unosTecnicos unAuto = filter (arreglarAuto unAuto) unosTecnicos

arreglarAuto :: Auto -> Tecnico -> Bool
arreglarAuto unAuto unTecnico = not.esUnAutoPeligroso.unTecnico $ unAuto 

-- Integrante b

costoDeReparacionDeAutosQueNecesitanRevision :: [Auto] -> [Int]
costoDeReparacionDeAutosQueNecesitanRevision unosAutos = sum (map costoDeReparacion.autosQueNecesitanRevision $ unosAutos)

autosQueNecesitanRevision :: [Auto] -> [Auto]
autosQueNecesitanRevision unosAutos = filter necesitaRevision unosAutos 

--7)

--Integrante a

-- Si, se puede hacer con una lista infinita ya que la funcion head trabaja con la estrategia de "evaluacion perezosa"
-- o call-by-name, entonces va a evaluar solamente los parametros que necesita (solo el primer elemento)
-- y "trabajar" a partir de eso. 

-- Ej.

primerTecnicoQueDejaElAutoEnCondiciones :: [Tecnico] -> Auto -> Tecnico
primerTecnicoQueDejaElAutoEnCondiciones unosTecnicos unAuto = head (tecnicosQueDejanElAutoEnCondiciones unosTecnicos unAuto)

-- El unico problema que podria presentarse es que no haya tecnicos que dejen el auto en condiciones, entonces nunca termina de evaluar.

--Integrante b

-- agregar la segunda respuesta

-- No se puede hacer con una lista infinita porque filter trabaja con la estrategia call-by-value o 
-- "evaluacion ansiosa" y los parametros tienen que resolverse antes de aplicar la funcion. En este
-- caso, como se trata de una lista infinita, es imposible obtener lo que buscamos. 

-- En caso de desear obtener solo los primeros 3 autos que necesitan revision, podriamos hacerlo de la siguiente forma:

primeros3AutosQueNecesitanRevision unosAutos = take 3 (filter necesitaRevision unosAutos)

-- Siempre y cuando haya 3 autos que necesiten revision ya que si hay menos la funcion seguira procesando la informacion
-- con el objetivo de encontrar los 3 autos


