! This program is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
! 
! This program is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
! FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License along with
! this program. If not, see <http://www.gnu.org/licenses/>.
!
!----------------------------------------------------------------------------
! Created By  : Daniel O. Perez
! Created Date: August of 2016
! email: perez.daniel.omar@gmail.com
! ---------------------------------------------------------------------------


module TraceReelHeaderModule

  use WordDefinitionModule, only:TRACE_WORD_NAME,TRACE_WORD_BYTE_SIZE,&
       EBCDIC_HEADER_SIZE,BINARY_HEADER_SIZE,TRACE_HEADER_SIZE
  use BinaryReelHeaderModule, only:readWordBinaryHeaderFromFile
  use ReadSegyToolsModule, only:checkOpenSegyFile
  implicit none

  interface readWordTraceHeader
     module procedure readWordTraceHeaderToArray, readWordTraceHeaderInFile
  end interface readWordTraceHeader

  interface changeWordTraceHeader
     module procedure changeWordTraceHeaderInArray, changeWordTraceHeaderInFile
  end interface changeWordTraceHeader
  
contains

  function readTraceHeaderToArray(filename,ntrace)

    !in 
    character(len=*), intent(in)::filename
    integer(kind=4), intent(in)::ntrace
    
    !out
    integer(kind=4), dimension(TRACE_HEADER_SIZE)::readTraceHeaderToArray

    !local
    integer(kind=4)::i,j,unit_number,flag,sum_bsize,nsamples
    integer(kind=4), dimension(1)::word_pos
    integer(kind=2)::word2b

    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call checkOpenSegyFile(filename,unit_number,flag)

    readTraceHeaderToArray=0
    
    do i=1,size(TRACE_WORD_NAME)
       
       !saples_number by traces, from binary header
       nsamples=readWordBinaryHeaderFromFile(filename,'ns')
       
       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(TRACE_WORD_BYTE_SIZE(1:i-1))
       select case(TRACE_WORD_BYTE_SIZE(i))
       case(2)
          read(unit=unit_number,&
               pos=EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+sum_bsize+1)word2b
          readTraceHeaderToArray(i)=int(word2b,kind=4)
       case(4)
          read(unit=unit_number,&
               pos=EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+sum_bsize+1)&
               readTraceHeaderToArray(i)
       end select
       
    end do
    
    if(flag==-1)close(unit_number)
    
    return
    
  end function readTraceHeaderToArray

  !=====================================================================

  !read binary header word from array
  function readWordTraceHeaderToArray(traceh_in,word_in)
    
    !in
    integer(kind=4), intent(in), dimension(:)::traceh_in
    character(len=*), intent(in)::word_in
    
    !out
    integer(kind=4)::readWordTraceHeaderToArray
    
    !local
    integer(kind=4)::i
    integer(kind=4), dimension(1)::word_pos
    
    
    !check if word exists
    if(any(TRACE_WORD_NAME==word_in)) then
       
       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       
       word_pos=pack([(i,i=1,size(TRACE_WORD_NAME))],TRACE_WORD_NAME==word_in)
       readWordTraceHeaderToArray=traceh_in(word_pos(1))
       
       return
       
    else
       
       write(*,*)"Error: no such binary word:",word_in
       stop
       
    end if
    
    
  end function readWordTraceHeaderToArray


  !=====================================================================

  !change binary header word from array
  subroutine changeWordTraceHeaderInArray(traceh_in,word_in,val_in)

    !in
    integer(kind=4), intent(inout), dimension(:)::traceh_in
    integer(kind=4), intent(in):: val_in
    character(len=*), intent(in)::word_in

    !local
    integer(kind=4)::i
    integer(kind=4), dimension(1)::word_pos
    
    !check if word exists
    if(any(TRACE_WORD_NAME==word_in)) then
       
       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always

       word_pos=pack([(i,i=1,size(TRACE_WORD_NAME))],TRACE_WORD_NAME==word_in)
       traceh_in(word_pos(1))=val_in

       return

    else

       write(*,*)"Error: no such binary word:",word_in
       stop

    end if

  end subroutine changeWordTraceHeaderInArray
  
  !=====================================================================

  subroutine writeTraceHeaderInFile(traceh_in,ntrace,filename)

    !in 
    character(len=*), intent(in):: filename
    integer(kind=4), intent(in), dimension(:)::traceh_in
    integer(kind=4), intent(in)::ntrace

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize,nsamples
    
    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call checkOpenSegyFile(filename,unit_number,flag)

    !saples_number by traces, from binary header
    nsamples=readWordTraceHeaderToArray(traceh_in,'ns')

    do i=1,size(TRACE_WORD_NAME)
                    
       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(TRACE_WORD_BYTE_SIZE(1:i-1))
      
       
       select case(TRACE_WORD_BYTE_SIZE(i))
       case(2)
          
          write(unit=unit_number,&
               pos=EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+sum_bsize+1)&
               int(traceh_in(i),kind=2)
         
       case(4)
       
          write(unit=unit_number,&
               pos=EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+sum_bsize+1)&
               traceh_in(i)
         
       end select
       
    end do

    !zeros to complete the header 
    do i=sum(TRACE_WORD_BYTE_SIZE),TRACE_HEADER_SIZE-1
       write(unit=unit_number,pos=EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+i+1)&
            int(0,kind=1)
    end do
    
    if(flag==-1)close(unit_number)       
         
    
  end subroutine writeTraceHeaderInFile
  
  !=====================================================================
  
  function readWordTraceHeaderInFile(filename,ntrace,word_in)

    !in 
    character(len=*), intent(in):: filename,word_in
    integer(kind=4), intent(in)::ntrace
    
    !out
    integer(kind=4)::readWordTraceHeaderInFile

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize,nsamples
    integer(kind=4), dimension(1)::word_pos
    integer(kind=2)::word2b
 

    sum_bsize=0
    
    !check if word exists
    if(any(TRACE_WORD_NAME==word_in)) then

       !check if filneame is connected to any unit
       !if flag==-1, a new unit was openend and must be closed at program end
       call checkOpenSegyFile(filename,unit_number,flag)

         
       !saples_number by traces, from binary header
       nsamples=readWordBinaryHeaderFromFile(filename,'ns')
       
       !check word_in position inside tracew_name
       word_pos=pack([(i,i=1,size(TRACE_WORD_NAME))],TRACE_WORD_NAME==word_in)
     
       !read word_in value as int*2 or int*4 according tracew_size
       !the ouptut is int*4 always
       sum_bsize=sum(TRACE_WORD_BYTE_SIZE(1:word_pos(1)-1))
       
       
       select case(TRACE_WORD_BYTE_SIZE(word_pos(1)))
       case(2)
          
          read(unit=unit_number,&
               pos=EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+sum_bsize+1)word2b
          readWordTraceHeaderInFile=int(word2b,kind=4)
          
       case(4)
       
          read(unit=unit_number,&
               pos=EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+sum_bsize+1)readWordTraceHeaderInFile
          
          
       end select

       if(flag==-1)close(unit_number)       

       return

    else

       write(*,*)"Error: no such trace word:",word_in
       stop

    end if


  end function readWordTraceHeaderInFile

  !=====================================================================

  subroutine changeWordTraceHeaderInFile(filename,ntrace,word_in,val_in)

    !in 
    character(len=*), intent(in):: filename,word_in
    integer(kind=4), intent(in):: ntrace,val_in

    !local
    integer(kind=4)::i,unit_number,flag,sum_bsize,nsamples
    integer(kind=4), dimension(1)::word_pos

    sum_bsize=0
    
    !check if word exists
    if(any(TRACE_WORD_NAME==word_in)) then

       !check if filneame is connected to any unit
       !if flag==-1, a new unit was openend and must be closed at program end
       call checkOpenSegyFile(filename,unit_number,flag)
       
       !saples_number by traces, from binary header
       nsamples=readWordBinaryHeaderFromFile(filename,'ns')

       !word position in TRACE_WORD_NAME array
       word_pos=pack([(i,i=1,size(TRACE_WORD_NAME))],TRACE_WORD_NAME==word_in)

       !read word_in value as int*2 or int*4 according binaryw_size
       !the ouptut is int*4 always
       sum_bsize=sum(TRACE_WORD_BYTE_SIZE(1:word_pos(1)-1))
       select case(TRACE_WORD_BYTE_SIZE(word_pos(1)))
       case(2)
          write(unit=unit_number,&
               pos=EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+sum_bsize+1)int(val_in,2)
       case(4)
          write(unit=unit_number,&
               pos=EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+sum_bsize+1)val_in
       end select
      
       
       if(flag==-1)close(unit_number) 

    else

       write(*,*)"Error: no such binary word:",word_in
       stop

    end if
    
  end subroutine changeWordTraceHeaderInFile
  
  !=========================================================================

  subroutine printTraceHeader(filename,ntrace,file_out)

    !in
    character(len=*), intent(in)::filename,file_out
    integer(kind=4), intent(in)::ntrace
    
    !local
    integer(kind=4)::unit_number,flag,i
    character(len=100), dimension(71)::def_array
    !check output unit
    if(file_out=='screen')then
       unit_number=6
       flag=0
    else
       open(newunit=unit_number,file=file_out,action='write')
       flag=-1
    end if
      
    def_array=(/ character(len=100)::'Trace sequence number within line',&
         'Trace sequence number within reel',&
         'Original field record number',&
         'Trace sequence number within original field record',&
         'Energy source point number',&
         'CDP ensemble number',&
         'Trace sequence number within CDP ensemble',&
         'Trace identification code',&
         'Number of vertically summed traces yielding this trace',&
         'Number of horizontally stacked traced yielding this trace',&
         'Data use (1 = production, 2 = test)',&
         'Distance from source point to receiver group',&
         'Receiver group elevation',&
         'Surface elevation at source',&
         'Source depth below surface',&
         'Datum elevation at receiver group',&
         'Datum elevation at source',&
         'Water depth at source',&
         'Water depth at receiver group',&
         'Scalar for elevations and depths (+ = multiplier, - = divisor)',&
         'Scalar for coordinates (+ = multiplier, - = divisor)',&
         'X source coordinate',&
         'Y source coordinate',&
         'X receiver group coordinate',&
         'Y receiver group coordinate',&
         'Coordinate units (1 = length in meters or feet, 2 = arc seconds)',&
         'Weathering velocity',&
         'Subweathering velocity',&
         'Uphole time at source',&
         'Uphole time at receiver group',&
         'Source static correction',&
         'Receiver group static correction',&
         'Total static applied',&
         'Lag time between end of header and time break in milliseconds',&
         'Lag time between time break and shot in milliseconds',&
         'Lag time beteen shot and recording start in milliseconds',&
         'Start of mute time',&
         'End of mute time',&
         'Number of samples in this trace',&
         'Sample interval of this trace in microseconds',&
         'Field instrument gain type code',&
         'Instrument gain constant',&
         'Intrument early gain in decibels.',&
         'Correlated (1 = no, 2 = yes)',&
         'Sweep frequency at start',&
         'Sweep fequency at end',&
         'Sweep length in milliseconds',&
         'Sweep type code',&
         'Sweep taper trace length at start in milliseconds',&
         'Sweep taper trace length at end in milliseconds',&
         'Taper type code',&
         'Alias filter frequency',&
         'Alias filter slope',&
         'Notch filter frequency',&
         'Notch filter slope',&
         'Low cut frequency',&
         'High cut frequency',&
         'Low cut slope',&
         'High cut slope',&
         'Year data recorded',&
         'Day of year',&
         'Hour of day (24-hour clock)',&
         'Minute of hour',&
         'Second of minute',&
         'Time basis (1 = local, 2 = GMT, 3 = other)',&
         'Trace weighting factor for fixed-point format data',&
         'Geophone group number of roll switch position one',&
         'Geophone group number of first trace of original field record',&
         'Geophone group number of last trace of original field record',&
         'Gap size (total number of groups dropped)',&
         'Overtravel associated with taper (1 = down/behind, 2 = up/ahead)'/)

    
    do i=1,size(def_array)
       write(unit_number,100)readWordTraceHeader(filename,ntrace,TRACE_WORD_NAME(i)),def_array(i)
    end do

100 format(I10,2X,A65)

    
  end subroutine printTraceHeader


  
end module TraceReelHeaderModule


  
