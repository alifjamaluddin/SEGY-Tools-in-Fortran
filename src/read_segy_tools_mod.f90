module read_segy_tools_mod

  implicit none
  integer(kind=2), parameter, dimension(256)::ebcdic_table = (/&
       Z'00', Z'01', Z'02', Z'03', Z'7F', Z'09', Z'7F', Z'7F',& 
       Z'7F', Z'7F', Z'7F', Z'0B', Z'0C', Z'0D', Z'0E', Z'0F',&
       Z'10', Z'11', Z'12', Z'13', Z'7F', Z'7F', Z'08', Z'7F',& 
       Z'18', Z'19', Z'7F', Z'7F', Z'1C', Z'1D', Z'1E', Z'1F',& 
       Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'0A', Z'17', Z'1B',& 
       Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'05', Z'06', Z'07',& 
       Z'7F', Z'7F', Z'16', Z'7F', Z'7F', Z'7F', Z'7F', Z'04',& 
       Z'7F', Z'7F', Z'7F', Z'7F', Z'14', Z'15', Z'7F', Z'1A',& 
       Z'20', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F',& 
       Z'7F', Z'7F', Z'5E', Z'2E', Z'3C', Z'28', Z'2B', Z'7C',& 
       Z'26', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F',& 
       Z'7F', Z'7F', Z'21', Z'24', Z'2A', Z'29', Z'3B', Z'7E',& 
       Z'2D', Z'2F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F',& 
       Z'7F', Z'7F', Z'7F', Z'2C', Z'25', Z'5F', Z'3E', Z'3F',& 
       Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F',& 
       Z'7F', Z'60', Z'3A', Z'23', Z'40', Z'27', Z'3D', Z'22',& 
       Z'7F', Z'61', Z'62', Z'63', Z'64', Z'65', Z'66', Z'67',& 
       Z'68', Z'69', Z'7F', Z'7B', Z'7F', Z'7F', Z'7F', Z'7F',& 
       Z'7F', Z'6A', Z'6B', Z'6C', Z'6D', Z'6E', Z'6F', Z'70',& 
       Z'71', Z'72', Z'7F', Z'7D', Z'7F', Z'7F', Z'7F', Z'7F',& 
       Z'7F', Z'7F', Z'73', Z'74', Z'75', Z'76', Z'77', Z'78',& 
       Z'79', Z'7A', Z'7F', Z'7F', Z'7F', Z'5B', Z'7F', Z'7F',& 
       Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F',& 
       Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'5D', Z'7F', Z'7F',& 
       Z'7B', Z'41', Z'42', Z'43', Z'44', Z'45', Z'46', Z'47',& 
       Z'48', Z'49', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F',& 
       Z'7D', Z'4A', Z'4B', Z'4C', Z'4D', Z'4E', Z'4F', Z'50',& 
       Z'51', Z'52', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F',& 
       Z'5C', Z'7F', Z'53', Z'54', Z'55', Z'56', Z'57', Z'58',& 
       Z'59', Z'5A', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F',& 
       Z'30', Z'31', Z'32', Z'33', Z'34', Z'35', Z'36', Z'37',& 
       Z'38', Z'39', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F', Z'7F'/) 
  
contains

  !==========================================================================

  function ebcdic_to_ascii(ebcdic_in)

    !in
    character(len=1)::ebcdic_in

    !out
    character(len=1)::ebcdic_to_ascii

    !local
    character(len=1)::aux
    integer (kind=4)::i
  
    ebcdic_to_ascii=char(ebcdic_table(ichar(ebcdic_in)+1))
  
    return
    
  end function ebcdic_to_ascii

  !==========================================================================

  function ascii_to_ebcdic(ascii_in)

    !in
    character(len=1)::ascii_in

    !out
    character(len=1)::ascii_to_ebcdic

    !local
    integer(kind=4)::i
    integer(kind=4), dimension(1)::pos
    
    pos=pack([(i,i=1,256)],ebcdic_table==(ichar(ascii_in)))
    ascii_to_ebcdic=char(pos(1)-1)
        
    
    return
    
  end function ascii_to_ebcdic

  !==========================================================================

  subroutine check_file_exists(file_in)
    
    character(len=*):: file_in
    ! variables de la info del archivo
    integer, dimension(13) :: buff
    integer:: status

    call stat(file_in, buff, status)
    if (status /= 0 )then
       write(*,*)"The file do not exists or is not readable."
       stop
    end if
    
  end subroutine check_file_exists

  !==========================================================================
    
  subroutine check_open_segy_file(file_in,unit_number,flag)
    
    character(len=*):: file_in
    integer(kind=4)::unit_number,flag
    
    !check if filneame is connected to any unit
    !if unit_name=-1 then file_in is not conected and then is opened,
    !otherwise is connected to unit_number
    inquire(file=file_in, number=unit_number)

    
    !if unit_number=-1 then open the file in a new unit
    !if a new unit is open, flag=-1
    !newunit option is used
    if(unit_number==-1)then
       flag=unit_number
       open(newunit=unit_number, &
            file=file_in,&
            form = 'unformatted', &
            access = 'stream',&
            convert = 'native')
    end if
    
  end subroutine check_open_segy_file
  
  
end module read_segy_tools_mod
