#program hanoi

#fungsi hanoi
def hanoi(n, A, B, C):
    if n == 1:
        print("Pindahkan donat", n, "dari tiang", A, "ke tiang", C)
    else :
        hanoi(n-1, A, C, B)
        print("pindahkan donat", n, "dari tiang", A, "ke tiang", C)
        hanoi(n-1, B, A, C)

#memulai perulagan 
while True:
    keputusan = input("apakah anda ingin memulainya ?")
    if keputusan == 'y':
        print('='*20)
        print("PROGRAM MENARA HANOI")
        print('='*20)
        #memasukan jumlah donat sesuai yang diinginkan
        n = int(input("masukan banyak donat yang diinginkan :"))
        #memanggil fungsi hanoi
        print(hanoi(n, 'A', 'B', 'C'))
    else:
        print("program anda selesai")
        break
