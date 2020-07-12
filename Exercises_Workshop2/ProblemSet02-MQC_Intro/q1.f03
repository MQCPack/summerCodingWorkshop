program q1

  use mqc_gaussian

  implicit none
  character(len=:),allocatable::fileName
  type(mqc_gaussian_unformatted_matrix_file)::fileInfo
  type(mqc_matrix)::alpha_MOs,density
  integer::nAlpha,nBasis
  logical::found

  call mqc_get_command_argument(1,fileName)
  write(6,'(A14,1X,A)') 'Input file is:',fileName
  call fileInfo%load(fileName)

  call fileInfo%getArray('ALPHA MO COEFFICIENTS',alpha_MOs,foundOut=found)
  call alpha_MOs%print(6,'alpha MOs')
  nAlpha = fileInfo%getVal('nalpha')
  nBasis = fileInfo%getVal('nbasis')
  alpha_MOs = alpha_MOs%mat([0],[1,nAlpha])
  call alpha_MOs%print(6,'occupied alpha MOs')
! P = C.C* 
  density = matmul(alpha_MOs,transpose(alpha_MOs))
  call density%print(6,'alpha density matrix')

end program q1
