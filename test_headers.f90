program test_readsegy

  use word_definitions_mod, only:n_ebcdich,n_binaryh
  use read_segy_tools_mod
  use ebcdic_reel_header_mod
  use binary_reel_header_mod
  use trace_reel_header_mod
  use traces_mod

  implicit none
  
  character(len=100)::file_in,file_out
  character(len=1), dimension(n_ebcdich)::ebcdic_header

  integer(kind=4), dimension(n_binaryh)::binary_header
  integer(kind=4), dimension(n_traceh)::trace_header
  integer(kind=4)::salida4b
  integer(kind=2)::salida2b
  integer::i,j,t_samples,t_sequence,n_trace
  
  ! 1- Abrir el archivo: 
  ! Esto es opcional. Si no se abre el archivo las subrutinas estan
  ! preparadas para abrir, trabajar y cerrar, pero esto consume
  ! tiempo. Es mejor abrirlo y cerrarlo al final del programa
  ! IMPORTANTE: ojo con la endianess

  file_in='entrada.sgy'
  file_out='salida.sgy'
  
  open(unit=100, &
       file=file_in,&
       form = 'unformatted', &
       access = 'stream',&
       convert = 'big_endian')

  open(unit=200, &
       file=file_out,&
       form = 'unformatted', &
       access = 'stream',&
       convert = 'native')
    
  !=====================================================================
  ! EBCDIC HEADER
  !=====================================================================

  ! function r_ebch_array:
  ! Esta funcion lee, letra a letra, el ebcdic header y lo guarda en un
  ! arreglo de dimension n_ebcdich=3200.
  !
  ! in: archivo sgy (binario) de entrada
  ! out: arreglo de dimension 3200 con el ebcdic header
  !
  ! por ejemplo, si se quiere leer el ebcdic header el archivo entrada.sgy:
  
  ebcdic_header=r_ebch_array(file_in)

  ! subroutine w_ebch_array:
  ! Esta funcion graba el ebcdic header en binario, en el inicio de un
  ! archivo (segy).
  !
  ! in: arreglo de dimension 3200 con un ebcdic header
  !     archivo sgy (binario) donde guardar el ebcdic header
  !
  ! Por ejemplo si se quiere guardar en el archivo 
  ! salida.sgy el header leido desde entrada.sgy se hace:

  call w_ebch_file(ebcdic_header,file_out)
  
  ! subroutine print_ebcdic_header:
  ! Esta subrutina lee un ebcdic header desde un archivo .seg, y lo guarda
  ! en un archivo ascii independiente, o lo muestra por pantalla
  !
  ! in: archivo sgy (binario) de donde leer un ebcdic header
  !     archivo donde guardar el ebidc header como ascii, si
  !     el archivo de salida es 'screen' se mustra por pantalla
  !

  ! guardar a un archivo 'ebidc_ascii.txt'
  call print_ebcdic_header(file_in,'ebcdic_ascii.txt')
  
  !mostrar por pantalla
  call print_ebcdic_header(file_in,'screen')


  
  !=====================================================================
  ! BINARY HEADER
  !=====================================================================

  ! function r_binh_array:
  ! Esta funcion lee el binary header y lo guarda en un
  ! arreglo de dimension n_binaryh
  !
  ! in: archivo sgy (binario) de entrada
  ! out: arreglo de dimensionn_binaryh con el binary header
  !
  ! por ejemplo, si se quiere leer el binary header el archivo entrada.sgy:

  binary_header=r_binh_array(file_in)

  ! subroutine w_binh_array:
  ! Esta funcion graba el binary header en binario, en  un
  ! archivo (segy).
  !
  ! in: arreglo con un binary header
  !     archivo sgy (binario) donde guardar el ebcdic header
  !
  ! Por ejemplo si se quiere guardar en el archivo 
  ! salida.sgy el header leido desde entrada.sgy se hace:

  call w_binh_file(binary_header,file_out)
  
  !=============================================================================
  ! Funcion rw_binh
  !
  ! Esta funcion hace diferentes cosas dependiendo de los valores de entrada!!
  ! En todos los casos lee una palabra del binary header.
  !============================================================================

  !Caso 1: Lee una palabra desde el arreglo leido previamente
  ! In: array con el binary hadar
  !     palabra a leer desde ese array
  ! la salida es un int*4

  t_samples=rw_binh(binary_header,'dt')
  !write(*,*)t_samples
  
  !Caso 2: Lee una palabra directamente desde el archivo
  ! In: nombre del archivo segy
  !   : palabra a leer desde el archivo
  ! la salida es un int*4
  
  t_samples=rw_binh(file_in,'dt')
  !write(*,*)t_samples

  !Caso 3: Lee una palabra de 2 bytes a un dado byte dentro del header
  ! In: nombre del archivo segy
  !     posicion de la palabra a leer, el valor tiene que ser de 2 bytes
  ! la salida es un int*2

  !se lee una palabra de 2 bytes que empieza en el 13-avo byte del header
  !corresponde al "Number of auxiliary traces per record"
  salida2b=rw_binh(file_in,int(13,2))
  !write(*,*)salida2b
  
  !Caso 3: Lee una palabra de 4 bytes a un dado byte dentro del header
  ! In: nombre del archivo segy
  !     posicion de la palabra a leer, el valor tiene que ser de 4bytes
  ! la salida es un int*4

  !se lee una palabra de 4 bytes que empieza en el 5-to byte del header
  !corresponde al  "Reel number"
  salida4b=rw_binh(file_in,int(5,4))
  !write(*,*)salida4b

  !============================================================================

  !Subrutina chw_binh_array:
  !Esta subrutina cambia una palabra del arreglo que contiene el binary header
  !
  ! in: arreglo con el binary header leido previamente
  !   : palabra a cambiar
  !   :nuevo valor de la palabra

  !por ejemplo, para cambiar el valor de dt a 4000 en el arreglo binary_header
  
  call chw_binh_array(binary_header,'dt',4000)
  t_samples=rw_binh_array(binary_header,'dt')
  !write(*,*)t_samples  !ahora la palabra dt vale 4000

  !Subrutina chw_binh_file:
  !Esta subrutina cambia una palabra del binary header directamente en el
  !archivo sgy
  !
  ! in: arhivo de entrada donde se cambia la palabra
  !   : palabra a cambiar
  !   : nuevo valor de la palabra

  !por ejemplo, para cambiar el valor de dt a 4000 en el archivo salida.sgy

  call chw_binh_file(file_out,'dt',4000)

  !subrutina prt_binh_file:
  ! Esta subrutina lee un binary header desde un archivo .seg, y lo guarda
  ! en un archivo ascii independiente, o lo muestra por pantalla
  !
  ! in: archivo sgy (binario) de donde leer un ebcdic header
  !     archivo donde guardar el ebidc header como ascii, si
  !     el archivo de salida es 'screen' se mustra por pantalla
  !

  ! guardar a un archivo 'ebidc_ascii.txt'
  call print_binary_header(file_out,'binary_ascii.txt')
  
  !mostrar por pantalla
  call print_binary_header(file_in,'screen')

  !=====================================================================
  !TRACE HEADERS
  !=====================================================================

  ! function r_trch_array:
  ! Esta funcion lee el header de una traza y lo guarda en un
  ! arreglo 
  !
  ! in: archivo sgy (binario) de entrada
  ! out: arreglo de dimension n_traceh con  header
  !
  ! por ejemplo, si se quiere leer el  header de la traza n_trace deentrada.sgy:

  n_trace=10
  
  trace_header=r_trch_array(file_in,n_trace)

  ! subroutine w_trch_array:
  ! Esta funcion graba el trace header en binario, en  un
  ! archivo (segy).
  !
  ! in: arreglo con un trace header
  !   : nro de traza
  !     archivo sgy (binario) donde guardar el trace header
  !
  ! Por ejemplo si se quiere guardar en el archivo 
  ! salida.sgy el header de la traza n_trace leido desde entrada.sgy se hace:
 
  call w_trch_file(trace_header,n_trace,file_out)
  
  !=============================================================================
  ! Funcion rw_trch
  !
  ! Esta funcion hace diferentes cosas dependiendo de los valores de entrada!!
  ! En todos los casos lee una palabra del trace header.
  !============================================================================

  !Caso 1: Lee una palabra desde el arreglo leido previamente
  ! In: array con el binary hadar
  !     palabra a leer desde ese array
  ! la salida es un int*4

  !ejemplo:   'CDP ensemble number'
  t_sequence=rw_trch(trace_header,'cdpen')
  !write(*,*)t_sequence
  
  !Caso 2: Lee una palabra directamente desde el archivo
  ! In: nombre del archivo segy
  !   : numero de traza
  !   : palabra a leer desde el archivo
  ! la salida es un int*4
  
  t_sequence=rw_trch(file_in,113,'cdpen')
  !write(*,*)t_sequence

  

  !=============================================================================
  ! Funcion chw_trch
  !
  ! Esta funcion hace diferentes cosas dependiendo de los valores de entrada!!
  ! En todos los casos cambia una palabra del trace header.
  !============================================================================

  !Caso 1: cambia una palabra desde el arreglo leido previamente
  ! In: array con el binary hadar
  !     palabra a cambiar desde ese array
  !     nuevo valor 

  !ejemplo:   'CDP ensemble number'
  call chw_trch(trace_header,'cdpen',100)
  t_sequence=rw_trch(trace_header,'cdpen')
  !write(*,*)t_sequence
  
  !Caso 2: cambia una palabra directamente desde el archivo
  ! In: nombre del archivo segy
  !   : numero de traza
  !   : palabra a cambiar en el archivo
  !   : nuevo valor
  
  call chw_trch(file_out,113,'cdpen',100)
  t_sequence=rw_trch(file_out,113,'cdpen')
 ! write(*,*)t_sequence

  
  !subrutina  print_trace_heade:
  ! Esta subrutina lee un binary header desde un archivo .seg, y lo guarda
  ! en un archivo ascii independiente, o lo muestra por pantalla
  !
  ! in: archivo sgy (binario) de donde leer un ebcdic header
  !     numero de traza
  !     archivo donde guardar el ebidc header como ascii, si
  !     el archivo de salida es 'screen' se mustra por pantalla
  !

  ! guardar a un archivo 'ebidc_ascii.txt'
  call  print_trace_header(file_out,n_trace,'trace_ascii.txt')
  
  !mostrar por pantalla
  call  print_trace_header(file_out,n_trace,'screen')
  
  
  close(100)
  close(200)

end program test_readsegy
