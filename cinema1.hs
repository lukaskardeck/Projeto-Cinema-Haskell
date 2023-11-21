import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import Text.Printf
import Data.List (find)


type Opcao = Int
type Codigo = Int
type Titulo = String
type Horarios = [String]
type Classificacao = Int
type Preco = Int
type Filme = (Codigo, Titulo, Horarios, Classificacao, Preco)
type InfoListFilme = (Codigo, Titulo, Horarios, Classificacao)
type InfoBuyFilme = (Codigo, Titulo, Horarios)
type QuantidadeIngressos = Int
type Ingresso = (Codigo, QuantidadeIngressos, Horarios)
type Cupom = (Titulo, Preco)


filmesDisponiveis :: [Filme]
filmesDisponiveis = [ (1, "Batman o retorno", ["14:00", "16:30", "19:00"], 14, 1500),
                      (2, "Pokemon 2", ["15:00", "18:30"], 9, 1500),
                      (3, "Homem Aranha 10", ["19:30", "21:00"], 16, 1500)
                    ]



formataCentavos :: Preco -> String
formataCentavos preco =
    show quo ++
    "." ++
    (if res < 10 then "0" else "") ++
    show res
    where
        (quo,res) = divMod preco 100




formatarHorarios :: Horarios -> String
formatarHorarios [] = ""
formatarHorarios [hora] = hora
formatarHorarios (hora:horas) = hora ++ ", " ++ formatarHorarios horas




getInfoListFilmes :: [Filme] -> [InfoListFilme]
getInfoListFilmes = map (\(cod, tit, hrs, clsf, _) -> (cod, tit, hrs, clsf))



getInfoBuyFilmes :: [Filme] -> [InfoBuyFilme]
getInfoBuyFilmes = map (\(cod, tit, hrs, _, _) -> (cod, tit, hrs))



printInfoListFilmes :: [InfoListFilme] -> String
printInfoListFilmes filmes =
    let header = "Código\tTítulo\t\t\tHorários\t\tClassificação\n"
        maxTituloLength = maximum $ map (length . (\(_, tit, _, _) -> tit)) filmes
        maxHorariosLength = maximum $ map (length . formatarHorarios . (\(_, _, hrs, _) -> hrs)) filmes
        alignTitulo tit = tit ++ replicate (maxTituloLength - length tit) ' '
        alignHorarios hrs = formatarHorarios hrs ++ replicate (maxHorariosLength - length (formatarHorarios hrs)) ' '
        linhaFilme (cod, tit, hrs, clsf) = show cod ++ "\t" ++ alignTitulo tit ++ "\t" ++ alignHorarios hrs ++ "\t" ++ show clsf ++ "\n"
    in header ++ 
    "---------------------------------------------------------------------\n" ++
    concatMap linhaFilme filmes ++
    "---------------------------------------------------------------------\n"



printInfoBuyFilmes :: [InfoBuyFilme] -> String
printInfoBuyFilmes filmes =
    let header = "Código\tTítulo\t\t\tHorários\n"
        maxTituloLength = maximum $ map (length . (\(_, tit, _) -> tit)) filmes
        maxHorariosLength = maximum $ map (length . formatarHorarios . (\(_, _, hrs) -> hrs)) filmes
        alignTitulo tit = tit ++ replicate (maxTituloLength - length tit) ' '
        alignHorarios hrs = formatarHorarios hrs ++ replicate (maxHorariosLength - length (formatarHorarios hrs)) ' '
        linhaFilme (cod, tit, hrs) = show cod ++ "\t" ++ alignTitulo tit ++ "\t" ++ alignHorarios hrs ++ "\n"
    in header ++
       "---------------------------------------------------------------------\n" ++
       concatMap linhaFilme filmes ++
       "---------------------------------------------------------------------\n"



ingressosComprados :: [Ingresso]
ingressosComprados = []


comprarIngressos :: IO ()
comprarIngressos = do
    let loopCompraIngressos :: [(Codigo, Int, [String])] -> IO ()
        loopCompraIngressos ingressosSoFar = do
            
            putStr "Digite o código do filme desejado (ou 0 para finalizar): "
            codigoFilme <- readLn

            if codigoFilme == 0 then do
                if null ingressosSoFar then do
                    putStrLn "Pedido cancelado. Nenhum ingresso foi comprado."
                    menu
                else do
                    -- Salvar os ingressos em um arquivo
                    writeFile "ingressos.txt" (show ingressosSoFar)
                    putStrLn "Pedido finalizado com sucesso!"
                    menu
            else if any (\(cod, _, _, _, _) -> cod == codigoFilme) filmesDisponiveis then do
                putStr "Digite a quantidade de ingressos desejada: "
                qtdIngressos <- readLn

                putStr "Digite o horário desejado: "
                horario <- getLine

                let ingresso = (codigoFilme, qtdIngressos, [horario])
                let novosIngressos = ingressosSoFar ++ [ingresso]

                putStrLn "Ingresso adicionado com sucesso!"

                putStr "Digite 1 para solicitar mais um ingresso ou 0 para finalizar o pedido: "
                escolha <- readLn :: IO Int
                if escolha == 1 then
                    loopCompraIngressos novosIngressos
                else do
                    -- Salvar os ingressos em um arquivo
                    writeFile "ingressos.txt" (show novosIngressos)
                    putStrLn "Pedido finalizado com sucesso!"
                    menu
            else do
                putStrLn "Código do filme inexistente!"
                loopCompraIngressos ingressosSoFar

    loopCompraIngressos []




calcularPrecoIngresso :: Ingresso -> Preco
calcularPrecoIngresso (codigoFilme, qtd, _) =
    case find (\(cod, _, _, _, _) -> cod == codigoFilme) filmesDisponiveis of
        Just (_, _, _, _, preco) -> preco * fromIntegral qtd
        Nothing -> 0


cupomFiscal :: IO ()
cupomFiscal = do
    putStrLn "\n============================================================================"
    putStrLn "\t\t\tCupom Fiscal"
    putStrLn "============================================================================\n"

    -- Ler dados do arquivo
    conteudoArquivo <- readFile "ingressos.txt"
    let ingressos = read conteudoArquivo :: [Ingresso]

    if null ingressos then do
        putStrLn "Você ainda não comprou nenhum ingresso."
    else do
        let calcularPrecoTotal = sum . map calcularPrecoIngresso
        let precoTotal = calcularPrecoTotal ingressos

        -- Exibir cupom fiscal
        mapM_ (\ingresso@(cod, _, _) -> putStrLn $ printCupomFiscal ingresso (calcularPrecoIngresso ingresso)) ingressos
        putStrLn $ "Total" ++ replicate (30 - length "Total") '.' ++ " R$" ++ formataCentavos precoTotal
    menu

printCupomFiscal :: Ingresso -> Preco -> String
printCupomFiscal (codigo, _, _) preco =
    case find (\(cod, tit, _, _, _) -> cod == codigo) filmesDisponiveis of
        Just (_, titulo, _, _, _) ->
            let precoIngresso = formataCentavos preco
            in titulo ++ replicate (30 - length titulo) '.' ++ " R$" ++ precoIngresso
        Nothing -> "Filme não encontrado"









-- Função para imprimir o menu
printMenu :: IO ()
printMenu = do
    putStrLn "\n\n\n\n============================================================================"
    putStrLn "\t\t\tCinema Haskell"
    putStrLn "============================================================================\n"
    putStrLn "Opções:"
    putStrLn "1. Listar Filmes \n2. Comprar Ingressos \n3. Cupom Fiscal \n4. Fim"




-- Função para ler uma opção do usuário
getOpcao :: IO Opcao
getOpcao = do
    putStr "Escolha uma opção: "
    x <- readLn
    return x




-- Função que direciona a escolha do usuário
escolhaUser :: Opcao -> IO ()
escolhaUser escolha =
    if escolha == 1 then do
        putStrLn "\n============================================================================"
        putStrLn "\t\t\tLista de Filmes"
        putStrLn "============================================================================\n"
        putStrLn $ printInfoListFilmes (getInfoListFilmes filmesDisponiveis)
        menu
    else if escolha == 2 then do
        putStrLn "\n============================================================================"
        putStrLn "\t\t\tCompre seu ingresso"
        putStrLn "============================================================================\n"
        putStrLn $ printInfoBuyFilmes (getInfoBuyFilmes filmesDisponiveis)
        comprarIngressos
    else if escolha == 3 then do
        cupomFiscal
    else if escolha == 4 then do
        putStrLn "\n============================================================================"
        putStrLn "\tFim da execução! Obrigado por escolher o Cinema Haskell !"
        putStrLn "============================================================================\n"
    else do
        putStrLn "Inválido!"
        opcao <- getOpcao
        escolhaUser opcao




-- Função que controla todo a interação no menu
menu :: IO ()
menu = do
    printMenu
    opcao <- getOpcao
    escolhaUser opcao


    

        
-- Função principal
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    menu
