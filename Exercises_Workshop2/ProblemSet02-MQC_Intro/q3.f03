program q3

  use mqc_gaussian

  implicit none
  character(len=:),allocatable::fileName
  type(mqc_gaussian_unformatted_matrix_file)::fileInfo
  type(mqc_scf_integral)::overlap,density
  type(mqc_scalar)::contract

  call mqc_get_command_argument(1,fileName)
  write(6,'(A14,1X,A)') 'Input file is:',fileName
  call fileInfo%load(fileName)

  call fileInfo%getESTObj('overlap',est_integral=overlap)
  call fileInfo%getESTObj('density',est_integral=density)
  call overlap%print(6,'overlap')
  call density%print(6,'density')
  contract = contraction(density,overlap)
  call contract%print(6,'<PS>')
  contract = mqc_scf_integral_trace(matmul(density,overlap))
  call contract%print(6,'trace(P.S)')

end program q3
