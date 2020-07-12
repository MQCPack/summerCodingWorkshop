program q2

  use mqc_gaussian

  implicit none
  character(len=:),allocatable::fileName
  type(mqc_gaussian_unformatted_matrix_file)::fileInfo
  type(mqc_scf_integral)::alpha_MOs,density
  integer::nAlpha,nBeta
  logical::found

  call mqc_get_command_argument(1,fileName)
  write(6,'(A14,1X,A)') 'Input file is:',fileName
  call fileInfo%load(fileName)

  call fileInfo%getESTObj('mo coefficients',est_integral=alpha_MOs,foundObj=found)
  call alpha_MOs%print(6,'alpha MOs')
  nAlpha = fileInfo%getVal('nalpha')
  nBeta = fileInfo%getVal('nbeta')
  alpha_MOs = alpha_MOs%orbitals('occupied',[nAlpha],[nBeta])
  call alpha_MOs%print(6,'occupied alpha MOs')
  density = matmul(alpha_MOs,dagger(alpha_MOs))
  call density%print(6,'alpha density matrix')

end program q2
