import Text.Printf (printf)

caculateEffectiveTemp :: Double -> Double -> Double
caculateEffectiveTemp realTemp baseTemp = realTemp - baseTemp

caculateRealK :: Double -> Double -> Double -> Double
caculateRealK nextK baseTemp nextlabTime = nextK - ((25 - baseTemp) * nextlabTime)

caculateRealTime :: Double -> Double -> Double
caculateRealTime realK effectiveTemp 
    | effectiveTemp == 0 = error "Error:除数是0啦！"
    | otherwise =realK / effectiveTemp

caculatePmiMIN :: Double -> Double -> Double
caculatePmiMIN realTime extraMIN = realTime + extraMIN

caculatePmiMAX :: Double -> Double -> Double
caculatePmiMAX realTime extraMAX= realTime + extraMAX

main :: IO ()
main = do 
    putStrLn "现场日均温："
    realTemp <- readLn :: IO Double

    putStrLn "发育起点温度："
    baseTemp <- readLn :: IO Double

    putStrLn "下一阶段所需积温："
    nextK <- readLn :: IO Double

    putStrLn "至下一阶段培养时间："
    nextlabTime <- readLn :: IO Double 

    putStrLn "定殖下限："
    extraMIN <- readLn :: IO Double 

    putStrLn "典型定殖上限："
    extraMAX <- readLn :: IO Double 

    if any (< 0) [realTemp, baseTemp, nextK, nextlabTime, extraMIN, extraMAX]
    then error "Error: 不应有负."
    else do

        let effectiveTemp = caculateEffectiveTemp realTemp baseTemp
            realK = caculateRealK nextK baseTemp nextlabTime
            realTime = caculateRealTime realK effectiveTemp
            pmiMIN = caculatePmiMIN realTime extraMIN
            pmiMAX = caculatePmiMAX realTime extraMAX

        printf "%.2f-%.2f\n" pmiMIN pmiMAX

