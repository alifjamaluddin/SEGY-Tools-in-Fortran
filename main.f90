
program main
    use WordDefinitionModule, only:EBCDIC_HEADER_SIZE,BINARY_HEADER_SIZE
    use ReadSegyToolsModule
    use EBCDICReelHeaderModule
    use BinaryReelHeaderModule
    use TraceReelHeaderModule
    use TracesModule

    character(len=300) :: arg, sub_arg, flag_val, output_path, input_path, help_msg, trace_word
    character(len=3) :: action
    integer :: num_args, i, n_trace, val
    logical :: action_set, output_set, input_set, n_trace_set,trace_word_set

    action_set = .False.
    output_set = .False.
    input_set = .False.
    n_trace_set = .False.
    trace_word_set = .False.


    help_msg = "./segytool input_path <option> -o output_path"//NEW_LINE('A')//"-pE print EBCDIC header"//NEW_LINE('A')//"-pB print Binary header"//NEW_LINE('A')//"-pT print Trace header"//NEW_LINE('A')//"-nt to set trace number, default=1"//NEW_LINE('A')//"-h print help"//NEW_LINE('A')//"-o output path, optional"



    ! Todo: read argument
    num_args = command_argument_count()
    if (num_args == 0) then
        print "(A)", help_msg
        call exit(0)
    end if

    i=1
    do
        call get_command_argument(i,arg)

        if (arg(:1)=='-') then
            select case(trim(arg))
            case('-o')
                if (i+1 <= num_args) then
                    i = i+1
                    call get_command_argument(i,arg)
                    ! "Output path"
                    output_path = adjustl(trim(arg))
                    output_set = .True.
                    open(unit=200, &
                    file=output_path,&
                    form = 'unformatted', &
                    access = 'stream',&
                    convert = 'native')
                end if
            case('-pE')
                action_set = .True.
                action = "pE"
            case('-pB')
                action_set = .True.
                action = "pB"
            case('-pT')
                action_set = .True.
                action = "pT"
            case('-nt')
                if (i+1 <= num_args) then
                    i = i+1
                    call get_command_argument(i,arg)
                    read(arg,"(I)") n_trace
                    n_trace_set = .True.
                end if
            case('-pTW')
                action_set = .True.
                action = "pTW"
                if (i+1 <= num_args) then
                    i = i+1
                    call get_command_argument(i,arg)
                    ! "Output path"
                    trace_word = adjustl(trim(arg))
                    trace_word_set = .True.
                end if
            case ('-h')
                print "(A)", help_msg
                call exit(0)


            case default
                write (*,*), "Unknown argument", arg
            end select
        else
            input_path = arg
            input_set = .True.
            open(unit=100, &
            file=input_path,&
            form = 'unformatted', &
            access = 'stream',&
            convert = 'big_endian')
        end if 

        i = i+1
        if (i > num_args) exit
    end do



    if (.not. action_set) then
        write (*,*), "No action defined"
    end if
    
    if (.not. input_set) then
        write (*,*), "No input defined"
    end if

   

    if (action_set .and. input_set) then
        select case(action)
        case ('pE')
            if(output_set) then
                call printEBCDICHeader(input_path,output_path)
            else
                call printEBCDICHeader(input_path,'screen')
            end if
        case ('pB')
            if(output_set) then
                call printBinaryHeader(input_path,output_path)
            else
                call printBinaryHeader(input_path,'screen')
            end if
        case ('pT')
            if(.not. n_trace_set) then
                n_trace = 1
            else
                if(output_set) then
                    call printTraceHeader(input_path,n_trace, output_path)
                else 
                    call printTraceHeader(input_path, n_trace, 'screen')
                end if
            end if
        case ('pTW')
            if(.not. n_trace_set) then
                n_trace = 1
            else
                val = readWordTraceHeaderInFile(input_path,n_trace,trace_word)
                print '(i)', val
            end if
          

            
        end select
      

       
    end if

    ! close file io
    if(input_set) close(100)
    if(output_set) close(200)



 
end program main