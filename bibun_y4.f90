program main
use omp_lib
implicit none

integer i,j,k,ix,iy,iz,mpij,myrank,nprocs
integer, parameter :: nxmax=1024,nymax=1024,nzmax=1024
real*8 ::x,y,z,dx,dy,dz,dxinv,dyinv,dzinv,y1,stime,etime
real*8, allocatable ::f(:,:,:),g(:,:,:)


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


stime=omp_get_wtime()
  dx=0.1d0
  dy=0.11d0
  dz=0.12d0
  dxinv=1.0d0/dx/dx
  dyinv=1.0d0/dy/dy
  dzinv=1.0d0/dz/dz

  do k=1,10
    do iz=1,nzmax
      do iy=1,nymax/4
        do ix=1,nxmax
          g(ix,iy,iz)=g(ix,iy,iz)+(f(ix+1,iy,iz)-2.0d0*f(ix,iy,iz)+f(ix-1,iy,iz))*dxinv+(f(ix,iy+1,iz)-2.0d0*f(ix,iy,iz)+f(ix,iy-1,iz))*dyinv+(f(ix,iy,iz+1)-2.0d0*f(ix,iy,iz)+f(ix,iy,iz-1))*dzinv
        end do
      end do
    end do


    do iz=1,nzmax
      do iy=nymax/4+1,nymax/2
        do ix=1,nxmax
          g(ix,iy,iz)=g(ix,iy,iz)+(f(ix+1,iy,iz)-2.0d0*f(ix,iy,iz)+f(ix-1,iy,iz))*dxinv+(f(ix,iy+1,iz)-2.0d0*f(ix,iy,iz)+f(ix,iy-1,iz))*dyinv+(f(ix,iy,iz+1)-2.0d0*f(ix,iy,iz)+f(ix,iy,iz-1))*dzinv
        end do
      end do
    end do


    do iz=1,nzmax
      do iy=nymax/2+1,3*nymax/4
        do ix=1,nxmax
          g(ix,iy,iz)=g(ix,iy,iz)+(f(ix+1,iy,iz)-2.0d0*f(ix,iy,iz)+f(ix-1,iy,iz))*dxinv+(f(ix,iy+1,iz)-2.0d0*f(ix,iy,iz)+f(ix,iy-1,iz))*dyinv+(f(ix,iy,iz+1)-2.0d0*f(ix,iy,iz)+f(ix,iy,iz-1))*dzinv
        end do
      end do
    end do


    do iz=1,nzmax
      do iy=3*nymax/4+1,nymax
        do ix=1,nxmax
          g(ix,iy,iz)=g(ix,iy,iz)+(f(ix+1,iy,iz)-2.0d0*f(ix,iy,iz)+f(ix-1,iy,iz))*dxinv+(f(ix,iy+1,iz)-2.0d0*f(ix,iy,iz)+f(ix,iy-1,iz))*dyinv+(f(ix,iy,iz+1)-2.0d0*f(ix,iy,iz)+f(ix,iy,iz-1))*dzinv
        end do
      end do
    end do

  end do

etime=omp_get_wtime()

write(6,*) etime-stime
write(6,*) 14.0d0*nxmax*nymax*nzmax*10.0d0/(etime-stime)/1.0E9


close(10)

stop
end
