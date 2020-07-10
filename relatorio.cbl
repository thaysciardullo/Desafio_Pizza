      * Divisão de identificação do programa
       Identification Division.
       Program-id. "relatorio".
       Author. "Thays Popper".
       Installation. "PC".
       Date-written. 08/07/2020.
       Date-compiled. 08/07/2020.


      * Divisão para configuração do ambiente
       Environment Division.
       Configuration Section.
           special-names. decimal-point is comma.

      * Declaração de recursos externos
       Input-output Section.
       File-control.
       I-O-Control.

      * Declaração de variáveis
       Data Division.
      *-----Variáveis de arquivos
       File Section.


      *----- Variáveis de trabalho
       working-storage section.

       01 relatorio occurs 20.
           05 nome                                  pic x(15)
                                                   value space.
           05 filler                                pic x(03)
              value "-".
           05 diametro                              pic 9(03).
           05 filler                                pic x(03)
              value "-".
           05 preco                                 pic 9(03)v99.
           05 filler                                pic x(03)
              value "-".
           05 preco_cm2                             pic 9(03)v99.
           05 filler                                pic x(03)
              value "-".
           05 diferenca_rel                        pic 9(04)v99.


       77 ind                                      pic 9(02).
       77 menu                                     pic x(01).
       77 pi                                       pic 9(01)v99
                                                    value 3,14.

       77 area_pizza                               pic 9(04)v99.
       77 raio                                     pic 9(04)v99.
       77 controle                                 pic x(10).
       77 aux                                      pic 9(10).
       77 qtd_pizza                                pic 9(04).
       77 delta_preco_cm2                          pic 9(03)v99.
      *---- Variáveis para comunicação entre programas
       linkage section.


      *-----Declaração de tela
       screen section.


      * Declaração do corpo do programa

       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

       inicializa section.

             move   'S'     to  menu
             move    1      to ind
             move    0      to area_pizza

           .
       inicializa-exit.
           exit.


      *      move  'broto'  to  nome(1).
      *      move   10      to  diametro(1).
      *      move   15,00   to  preco(1).
      *      move   0       to  preco_cm2(1).
      *      move   0       to  diferenca_rel(1).


      *      move  'pequena'  to  nome(2).
      *      move   25        to  diametro(2).
      *      move   25,00     to  preco(2).
      *      move   0         to  preco_cm2(2).
      *      move   0         to  diferenca_rel(2).


      * podemos calcular assim  -- compute total = preco (1) + margem.

      *     display nome(1).



      *------------- Se fizer assim, você tem que chamar pelo relatorio
      * e entra todas as variaveis do relatório, isso seria vetor/matriz
      * Seria para Criar Tabelas.
       processamento section.

            display relatorio(1).
               move 0 to ind
            perform until menu <> 'S'
               display erase
               add 1 to ind

               if ind > 20 then
                   display "Voce Atingiu o Limite de 20 Pizzas"
               else

               display "Informe o nome da pizza"
               accept nome(ind)

               display " Informe o diametro"
               accept diametro(ind)

               display " Informe o preco"
               accept preco(ind)

               end-if

               perform calculo-preco-cm2




               display " Deseja Cadastrar Mais Uma Pizza? 'S'/'N'"
               accept menu
            end-perform

            perform ordenar
            perform calculo-porcent



            perform varying ind from 1 by 1 until ind > 20
                                            or nome(ind) = space

                display relatorio(ind)

            end-perform


             .
       processamento-exit.
           exit.


      *------ Calculo Preço cm2---------------
       calculo-preco-cm2 section.

           compute raio = diametro(ind)/2
           compute area_pizza = pi * (raio * raio)



           compute preco_cm2(ind) = preco(ind) / area_pizza


           .



       calculo-preco-cm2-exit.

           exit.


      *-- Ordenação---------------------------------------
       ordenar section.
           move 'trocou' to controle

           perform until controle <> 'trocou'
               move 1 to ind
               move 'N trocou' to controle
               perform until ind = 20
                          or nome (ind + 1) = space

                   if preco_cm2(ind) > preco_cm2(ind + 1) then
                       move preco_cm2(ind + 1)  to aux
                       move preco_cm2(ind)      to preco_cm2(ind + 1)
                       move aux                 to preco_cm2(ind)
                       move 'trocou'            to controle


                   end-if
                   add 1 to ind
               end-perform
             end-perform


           .
       ordenar-exit.
           exit.


      *---- Calculo Porcentagem-------------------------------

       calculo-porcent section.
           move  1   to  ind

           perform until ind = 20
                   or nome(ind + 1) = space

           compute delta_preco_cm2= preco_cm2 (ind + 1) -
                                           preco_cm2(ind)

           compute diferenca_rel(ind + 1) = (delta_preco_cm2 * 100)/
                                             preco_cm2(ind)
           add 1 to ind
          end-perform

           .
       calculo-porcent-exit.
           exit.



       finaliza section.

           stop run
           .
       finaliza-exit.
           exit.










