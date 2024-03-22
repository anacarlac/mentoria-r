# Para usar essa calculadora, clique no botão SOURCE no canto superior direito do Script
# Após clicar em SOURCE, digite as respostas para as perguntas que aparecerem no CONSOLE
# Se quiser inserir um valor diferente, clique em SOURCE novamente.

calculadora <- function() {
  print("Um taco de beisebol e uma bola custam um total de R$1.10. Se o taco custa R$1 a mais do que a bola, quanto custa a bola?")
  print("R$0.05")
  print("R$0.10")
  print("R$1.00")
  print("R$1.05")
  num1 = as.numeric(readline(prompt="Digite o valor da bola em reais (sem cifrão):"))
  result <- num1+1.05

  if(num1==0.05) {
    print((paste("Você disse que o valor é: R$",num1,". Isso dá um total de R$",result,". Parabéns, você acertou!")))
  }
  else{
    print((paste("Você disse que o valor é: R$",num1,". Isso dá um total de R$",result,". Parece que algo deu errado, né? O seu total deveria ser R$1.10. Tente de novo.")))
  }
} 

calculadora()

