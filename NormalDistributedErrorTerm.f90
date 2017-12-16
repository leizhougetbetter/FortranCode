real function NormalDistError()
            !!!! generate the error from a normal distribution with zero mean and sigma std
            !!!! Reference to h ttp://c-faq.com/lib/gaussian.html
            !!!! Using Polar form proposed by Marsaglia
            !!!!
            implicit none
            real::rand01, rand02, NormalSquareSum
            !!!
            !!call RANDOM_NUMBER(randNormal)
            if(NormalRandNotUse)then
                NormalSquareSum = 0.0
                do while(abs(NormalSquareSum) < 1e-6 .or. NormalSquareSum >= 1)
                    rand01 = 2 * grnd() - 1.0
                    rand02 = 2 * grnd() - 1.0
                    NormalSquareSum = rand01 * rand01 + rand02 * rand02
                end do
                twoNormalRand(1) = rand01 * SQRT(-2.0 * LOG(NormalSquareSum) / NormalSquareSum)
                twoNormalRand(2) = rand02 * SQRT(-2.0 * LOG(NormalSquareSum) / NormalSquareSum)
                NormalRandNotUse = .false.
                NormalDistError = stdNormal * twoNormalRand(1)
            else
                NormalDistError = stdNormal * twoNormalRand(2)
                NormalRandNotUse = .true.
            end if
            !!!!
            return
        end function NormalDistError
