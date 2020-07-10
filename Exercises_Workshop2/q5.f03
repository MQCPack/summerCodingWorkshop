program q5

  use mqc_gaussian

  implicit none
  character(len=:),allocatable::fileName
  type(mqc_gaussian_unformatted_matrix_file)::fileInfo
  type(mqc_scf_integral)::mo_coeffs
  type(mqc_matrix)::sh2AtMp,shlTyp,nPrmSh,prmExp,conCoef,conCoTwo,shCoor
  type(mqc_scf_eigenvalues)::mo_energies

! Open old matrix file
  call mqc_get_command_argument(1,fileName)
  write(6,'(A14,1X,A)') 'Input file is:',fileName
  call fileInfo%load(fileName)

! Read required information off old matrix file
  call fileInfo%getESTObj('mo coefficients',est_integral=mo_coeffs)
  call fileInfo%getESTObj('mo energies',est_eigenvalues=mo_energies)
  call fileInfo%getArray('SHELL TO ATOM MAP',sh2AtMp)
  call fileInfo%getArray('SHELL TYPES',shlTyp)
  call fileInfo%getArray('NUMBER OF PRIMITIVES PER SHELL',nPrmSh)
  call fileInfo%getArray('PRIMITIVE EXPONENTS',prmExp)
  call fileInfo%getArray('CONTRACTION COEFFICIENTS',conCoef)
  call fileInfo%getArray('P(S=P) CONTRACTION COEFFICIENTS',conCoTwo)
  call fileInfo%getArray('COORDINATES OF EACH SHELL',shCoor)

! Create the new matrix file
  call fileInfo%create('new_matrix_file.mat')

! Write out all information to the matrix file
  call fileInfo%writeESTObj('mo coefficients',est_integral=mo_coeffs)
  call fileinfo%writeESTObj('mo energies',est_eigenvalues=mo_energies)
  call fileInfo%writeArray('SHELL TO ATOM MAP',sh2AtMp)
  call fileInfo%writeArray('SHELL TYPES',shlTyp)
  call fileInfo%writeArray('NUMBER OF PRIMITIVES PER SHELL',nPrmSh)
  call fileInfo%writeArray('PRIMITIVE EXPONENTS',prmExp)
  call fileInfo%writeArray('CONTRACTION COEFFICIENTS',conCoef)
  call fileInfo%writeArray('P(S=P) CONTRACTION COEFFICIENTS',conCoTwo)
  call fileInfo%writeArray('COORDINATES OF EACH SHELL',shCoor)

! Transform matrix file to checkpoint file
  call EXECUTE_COMMAND_LINE("unfchk -matrix new_matrix_file.mat new_matrix_file.chk")

end program q5
