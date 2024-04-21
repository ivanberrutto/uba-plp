
#Agarro los cases k<50
cases = int(input())
for i in range(cases):
    jugadores = []
    for j in range(10):
        jugador = list(map(str, input().split(" ")))
        jugador[1] = int(jugador[1]) # la pos 1 indica el ataque
        jugador[2] = int(jugador[2]) # la pos 2 indica la defensa
        jugadores.append(jugador) # agrega el jugador a la lista de jugadores
    #ordeno el equipazo segun mayor ataque, menor defensa y orden lexicografico )
    jugadores = sorted(jugadores, key=lambda x: (-x[1], x[2], x[0]))
    #los pongo en sus posiciones y los ordeno alfabeticamente
    jugadores = sorted(jugadores[0:5]) + sorted(jugadores[5:10])
    #imprimo el equipazo de dieguito maradona
    print(f"Case {i+1}:")
    for k in range(10):
        if k==0:
            print("(", end="")
        if k==5:
            print(")")
            print("(", end="")

        print(f"{jugadores[k][0]}", end="")
        if (k!=4 and k!= 9):
            print(", ", end="")

    print(")")


    
    
    