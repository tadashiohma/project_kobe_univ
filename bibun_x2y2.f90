implicit none
include 'mpif.h'
integer i,j,k,ix,iy,iz,mpij,myrank,nprocs
integer, parameter :: nxmax=256,nymax=256,nzmax=256
real*8 ::x,y,z,dx,dy,dz,dxinv,dyinv,dzinv,y1,stime,etime
real*8, allocatable ::f(:,:,:),g(:,:,:)
call mpi_init(mpij)
call mpi_comm_size(mpi_comm_world,nprocs,mpij)
call mpi_comm_rank(mpi_comm_world,myrank,mpij)


allocate(f(0:nxmax+1,0:nymax+1,0:nzmax+1),g(0:nxmax+1,0:nymax+1,0:nzmax+1))

open(10,file='data1.txt',form='formatted')

do iz=0,nzmax+1
  do iy=0,nymax+1
    do ix=0,nxmax+1
      dx=0.1d0
      dy=0.11d0
      dz=0.12d0
      x=ix*dx
      y=iy*dy
      z=iz*dz
      f(ix,iy,iz)=x*x + y*y + z*z
      g(ix,iy,iz)=0.0d0
    end do
  end do
end do


stime=mpi_wtime()
  dx=0.1d0
  dy=0.11d0
  dz=0.12d0
  dxinv=1.0d0/dx/dx
  dyinv=1.0d0/dy/dy
  dzinv=1.0d0/dz/dz

  do k=1,30

!2<ix<nxmax/2-1,2<iy<nymax/2-1
    do iz=1,nzmax
      do iy=1,nymax/2
        do ix=1,nxmax/2
          g(ix,iy,iz)=g(ix,iy,iz)+(f(ix+1,iy,iz)-2.0d0*f(ix,iy,iz)+f(ix-1,iy,iz))*dxinv+(f(ix,iy+1,iz)-2.0d0*f(ix,iy,iz)+f(ix,iy-1,iz))*dyinv+(f(ix,iy,iz+1)-2.0d0*f(ix,iy,iz)+f(ix,iy,iz-1))*dzinv
        end do
      end do
    end do

!nxmax/2<ix<nxmax-1,2<iy<nymax/2-1
    do iz=1,nzmax
      do iy=1,nymax/2
        do ix=nxmax/2+1,nxmax
          g(ix,iy,iz)=g(ix,iy,iz)+(f(ix+1,iy,iz)-2.0d0*f(ix,iy,iz)+f(ix-1,iy,iz))*dxinv+(f(ix,iy+1,iz)-2.0d0*f(ix,iy,iz)+f(ix,iy-1,iz))*dyinv+(f(ix,iy,iz+1)-2.0d0*f(ix,iy,iz)+f(ix,iy,iz-1))*dzinv
        end do
      end do
    end do

!2<ix<nxmax/2-1,nymax/2<iy<nymax-1
    do iz=1,nzmax
      do iy=nymax/2,nymax
        do ix=1,nxmax/2
          g(ix,iy,iz)=g(ix,iy,iz)+(f(ix+1,iy,iz)-2.0d0*f(ix,iy,iz)+f(ix-1,iy,iz))*dxinv+(f(ix,iy+1,iz)-2.0d0*f(ix,iy,iz)+f(ix,iy-1,iz))*dyinv+(f(ix,iy,iz+1)-2.0d0*f(ix,iy,iz)+f(ix,iy,iz-1))*dzinv
        end do
      end do
    end do

!nxmax/2<ix<nxmax-1,nymax/2<iy<nymax-1
    do iz=1,nzmax
      do iy=nymax/2,nymax
        do ix=nxmax/2+1,nxmax

          g(ix,iy,iz)=g(ix,iy,iz)+(f(ix+1,iy,iz)-2.0d0*f(ix,iy,iz)+f(ix-1,iy,iz))*dxinv+(f(ix,iy+1,iz)-2.0d0*f(ix,iy,iz)+f(ix,iy-1,iz))*dyinv+(f(ix,iy,iz+1)-2.0d0*f(ix,iy,iz)+f(ix,iy,iz-1))*dzinv
        end do
      end do
    end do



  end do
etime=mpi_wtime()
write(6,*) etime-stime,14.0d0*nxmax*nymax*nzmax*30.0d0/(etime-stime)/1.0E9


close(10)

call mpi_finalize(mpij)
stop
end
