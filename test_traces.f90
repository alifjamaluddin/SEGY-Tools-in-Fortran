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
  integer::i,j,n_samples,n_traces

  real(kind=4), allocatable:: trace(:)
  
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
       convert = 'big_endian')

  !voy a contar la cantidad de trazas que tiene el archivo de entrada:

  !function count_traces:
  ! in: file_in
  !   : numero de muestras por traza
  ! out: numero de trazas en file_in

  !necesito leer el numero de muestras del binary header
  !para eso uso la función rw_binh, que me permite leer una palabra del binary header
  !en este caso me interesa 'ns'
  n_samples=rw_binh(file_in,'ns')
  n_traces=count_traces(file_in,n_samples)
  write(*,*)'El nro de trazas es:', n_traces

  
  !voy a multiplicar las trazas del archivo de entrada por 10,
  !y voy a guardarlas en un nuevo archivo que va a tener todos
  !los mismos headers que el original

  !Leo el ebdic header y lo guardo en un arreglo
  ebcdic_header=r_ebch_array(file_in)
  !lo guardo en el archivo de salida
  call w_ebch_file(ebcdic_header,file_out)
  !esto tambien se podría hacer asi: call w_ebch_file(r_ebch_array(file_in),file_out)

  !Leo el binary header y lo guardo en un arreglo
  binary_header=r_binh_array(file_in)
  !lo guardo en el archivo de salida
  call w_binh_file(binary_header,file_out)
  !esto tambien se podría hacer asi: call w_binh_file(r_binh_array(file_in),file_out)

  !para cada traza del archivo de entrada, leo el header, la traza
  !multiplico la traza por 10, y guardo ambas cosas en la salida
  
  allocate(trace(n_samples))
  do j=1,n_traces

     !leo el trace header de la traza correspondiente
     trace_header=r_trch_array(file_in,j)
     !guardo el trace header en la salida
     call w_trch_file(trace_header,j,file_out)
     !esto tambien se podría hacer asi: call w_trch_file(r_trch_array(file_in),file_out)
     
     !leo las trazas y las guardo en el arreglo traces
     !se lee de una traza a la vez usando la funcion n_trace
     !function r_trace:
     ! in: file_in
     !     numero de traza
     !     primera muestra
     !     ultima muesta
     trace(:)=r_trace(file_in,j,1,n_samples)

     !modifico la traza y la guard
     !se usa la subrutina w_trace
     !subrutina w_trace:
     ! in: file_in
     !     traza
     !     numero de traza
     call w_trace(file_out,trace*10,j)
     
  end do
     
  deallocate(trace)
  
  close(100)
  close(200)

end program test_readsegy
