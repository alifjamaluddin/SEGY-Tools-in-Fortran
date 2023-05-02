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


module TracesModule

  use WordDefinitionModule, only:TRACE_WORD_NAME,TRACE_WORD_BYTE_SIZE,&
       EBCDIC_HEADER_SIZE,BINARY_HEADER_SIZE,TRACE_HEADER_SIZE
  use TraceReelHeaderModule, only:readWordTraceHeaderInFile
  use ReadSegyToolsModule, only:checkOpenSegyFile
  
  implicit none

contains

  function readTrace(filename,ntrace,nsmin,nsmax)

    !in 
    character(len=*), intent(in)::filename
    integer(kind=4), intent(in)::ntrace,nsmin,nsmax

    !out
    real(kind=4), dimension(nsmax-nsmin+1)::readTrace

    !local
    integer(kind=4)::flag,unit_number,i,nsamples
    
    !check if smin<=0 and smax>nsamples
    nsamples=readWordTraceHeaderInFile(filename,ntrace,'ns')
    if((nsmin.lt.1).or.(nsmax.gt.nsamples))then
       write(*,*)'Range (nsmin,nsmax) out of trace boundaries'
       stop
    end if

    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call checkOpenSegyFile(filename,unit_number,flag)

    !read trace samples
    do i=nsmin,nsmax
       read(unit=unit_number,&
            pos=(EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+TRACE_HEADER_SIZE)+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+1+4*(i-1))readTrace(i-nsmin+1)
    end do
       

    if(flag==-1)close(unit_number) 

    return
    
  end function readTrace

  !==========================================================================================================

  subroutine writeTrace(filename,trace,ntrace)

    !in
    character(len=*), intent(in)::filename
    integer(kind=4), intent(in)::ntrace
    real(kind=4), intent(in), dimension(:)::trace
    
    !local
    integer(kind=4)::flag,unit_number,i,nsamples


    nsamples=size(trace)
    
    !check if filneame is connected to any unit
    !if flag==-1, a new unit was openend and must be closed at program end
    call checkOpenSegyFile(filename,unit_number,flag)  

    !write trace samples to file
    do i=1,nsamples
       write(unit=unit_number,&
            pos=(EBCDIC_HEADER_SIZE+BINARY_HEADER_SIZE+TRACE_HEADER_SIZE)+(ntrace-1)*(TRACE_HEADER_SIZE+4*nsamples)+1+4*(i-1))trace(i)
    end do
       

    if(flag==-1)close(unit_number) 
    
  end subroutine writeTrace


  function countTraces(filename,ns)

    !in
    character(len=*), intent(in)::filename
    integer(kind=4), intent(in)::ns

    !out
    integer(kind=4)::countTraces

    !local
    integer, dimension(13)::buff
    integer:: status
    
    call stat(filename, buff, status)

    countTraces=(buff(8)-EBCDIC_HEADER_SIZE-BINARY_HEADER_SIZE)/(TRACE_HEADER_SIZE+ns*4)
    
    return

  end function countTraces

  function maxValTraces(filename,tmin,tmax,smin,smax)

    !in
    character(len=*), intent(in)::filename
    integer(kind=4), intent(in)::tmin,tmax,smin,smax

    !out
    integer(kind=4)::maxValTraces

    !local
    integer::i
    real(kind=4)::mval_tmp
    
    maxValTraces=maxval(abs(readTrace(filename,tmin,smin,smax)))
    do i=tmin+1,tmax
       mval_tmp=maxval(abs(readTrace(filename,i,smin,smax)))
       if(mval_tmp.gt.maxValTraces)maxValTraces=mval_tmp
    end do
    
  end function maxValTraces

  
end module TracesModule

