################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../battleships.f90 

OBJS += \
./battleships.o 


# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O3 -Wall -c -fmessage-length=0 -static -v -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

battleships.o: ../battleships.f90


