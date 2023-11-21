import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import Text.Printf

type Opcao = Int
type Codigo = Int
type Titulo = String
type Horarios = [String]
type Classificacao = Int
type Preco = Int
type Filme = (Codigo, Titulo, Horarios, Classificacao, Preco)
type InfoListFilme = (Codigo, Titulo, Horarios, Classificacao)
type InforBuyFilme = (Codigo, Titulo, Horarios)


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




obterInfoFilmes :: [Filme] -> [InfoListFilme]
obterInfoFilmes = map (\(cod, tit, hrs, clsf, _) -> (cod, tit, hrs, clsf))


printTabelaFilmes :: [(Codigo, Titulo, Horarios, Classificacao)] -> String
printTabelaFilmes filmes =
    let header = "Código\tTítulo\t\t\tHorários\t\tClassificação\n"
        maxTituloLength = maximum $ map (length . (\(_, tit, _, _) -> tit)) filmes
        maxHorariosLength = maximum $ map (length . formatarHorarios . (\(_, _, hrs, _) -> hrs)) filmes
        alignTitulo tit = tit ++ replicate (maxTituloLength - length tit) ' '
        alignHorarios hrs = formatarHorarios hrs ++ replicate (maxHorariosLength - length (formatarHorarios hrs)) ' '
        linhaFilme (cod, tit, hrs, clsf) = show cod ++ "\t" ++ alignTitulo tit ++ "\t" ++ alignHorarios hrs ++ "\t" ++ show clsf ++ "\n"
    in header ++ 
    "---------------------------------------------------------------------\n" ++
    concatMap linhaFilme filmes



-- Função para imprimir o menu
printMenu :: IO ()
printMenu = do
    putStrLn "\n========================="
    putStrLn "Cinema Haskell"
    putStrLn "==========================\n"
    putStrLn "Opções:"
    putStrLn "1. Listar Filmes \n2. Comprar Ingressos \n3. Cupom Fiscal \n4. Fim"




-- Função para ler uma opção do usuário
leOpcao :: IO Opcao
leOpcao = do
    putStr "Escolha uma opção: "
    x <- readLn
    return x




-- Função que direciona a escolha do usuário
escolhaUser :: Opcao -> IO ()
escolhaUser escolha =
    if escolha == 1 then do
        putStrLn "\n============================================================================"
        putStrLn "\t\t\tFilmes Disponíveis"
        putStrLn "============================================================================\n"
        putStrLn $ printTabelaFilmes (obterInfoFilmes filmesDisponiveis)
        --let infoFilm = obterInfoFilmes filmesDisponiveis
        --print infoFilm
        menu
    else if escolha == 2 then do
        putStrLn "Você escolheu 2"
        menu
    else if escolha == 3 then do
        putStrLn "Você escolheu 3"
        menu
    else if escolha == 4 then do
        putStrLn "\n============================================================================"
        putStrLn "\tFim da execução! Obrigado por escolher o Cinema Haskell !"
        putStrLn "============================================================================\n"
    else do
        putStrLn "Inválido!"
        opcao <- leOpcao
        escolhaUser opcao




-- Função que controla todo a interação no menu
menu :: IO ()
menu = do
    printMenu
    opcao <- leOpcao
    escolhaUser opcao


    

        
-- Função principal
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    menu
    --putStrLn "SLA"
