module Interval
    exposing
        ( IntervalName(..)
        , Interval
        , interval
        , semitones
        , noteNameSteps
        )


type IntervalName
    = PerfectUnison
    | AugmentedUnison
    | MajorSecond
    | MinorSecond
    | AugmentedSecond
    | DiminishedSecond
    | MajorThird
    | MinorThird
    | AugmentedThird
    | DiminishedThird
    | PerfectFourth
    | AugmentedFourth
    | DiminishedFourth
    | PerfectFifth
    | AugmentedFifth
    | DiminishedFifth
    | MajorSixth
    | MinorSixth
    | AugmentedSixth
    | DiminishedSixth
    | MajorSeventh
    | MinorSeventh
    | AugmentedSeventh
    | DiminishedSeventh
    | PerfectOctave
    | AugmentedOctave
    | DiminishedOctave


type Interval
    = Unison PerfectQuality
    | Second ImperfectQuality
    | Third ImperfectQuality
    | Fourth PerfectQuality
    | Fifth PerfectQuality
    | Sixth ImperfectQuality
    | Seventh ImperfectQuality
    | Octave PerfectQuality


type Quality
    = Perfect
    | Major
    | Minor
    | Augmented
    | Diminished


type PerfectQuality
    = PerfectPQ
    | AugmentedPQ
    | DiminishedPQ


type ImperfectQuality
    = MajorIQ
    | MinorIQ
    | AugmentedIQ
    | DiminishedIQ


interval : IntervalName -> Interval
interval intervalName =
    case intervalName of
        PerfectUnison ->
            Unison PerfectPQ

        AugmentedUnison ->
            Unison AugmentedPQ

        MajorSecond ->
            Second MajorIQ

        MinorSecond ->
            Second MinorIQ

        AugmentedSecond ->
            Second AugmentedIQ

        DiminishedSecond ->
            Second DiminishedIQ

        MajorThird ->
            Third MajorIQ

        MinorThird ->
            Third MinorIQ

        AugmentedThird ->
            Third AugmentedIQ

        DiminishedThird ->
            Third DiminishedIQ

        PerfectFourth ->
            Fourth PerfectPQ

        AugmentedFourth ->
            Fourth AugmentedPQ

        DiminishedFourth ->
            Fourth DiminishedPQ

        PerfectFifth ->
            Fifth PerfectPQ

        AugmentedFifth ->
            Fifth AugmentedPQ

        DiminishedFifth ->
            Fifth DiminishedPQ

        MajorSixth ->
            Sixth MajorIQ

        MinorSixth ->
            Sixth MinorIQ

        AugmentedSixth ->
            Sixth AugmentedIQ

        DiminishedSixth ->
            Sixth DiminishedIQ

        MajorSeventh ->
            Seventh MajorIQ

        MinorSeventh ->
            Seventh MinorIQ

        AugmentedSeventh ->
            Seventh AugmentedIQ

        DiminishedSeventh ->
            Seventh DiminishedIQ

        PerfectOctave ->
            Octave PerfectPQ

        AugmentedOctave ->
            Octave AugmentedPQ

        DiminishedOctave ->
            Octave DiminishedPQ


noteNameSteps : Interval -> Int
noteNameSteps interval =
    case interval of
        Unison _ ->
            0

        Second _ ->
            1

        Third _ ->
            2

        Fourth _ ->
            3

        Fifth _ ->
            4

        Sixth _ ->
            5

        Seventh _ ->
            6

        Octave _ ->
            7


semitones : Interval -> Int
semitones interval =
    let
        imperfectQualityNumber : ImperfectQuality -> Int
        imperfectQualityNumber quality =
            case quality of
                MajorIQ ->
                    0

                MinorIQ ->
                    -1

                AugmentedIQ ->
                    1

                DiminishedIQ ->
                    -2

        perfectQualityNumber : PerfectQuality -> Int
        perfectQualityNumber quality =
            case quality of
                PerfectPQ ->
                    0

                AugmentedPQ ->
                    1

                DiminishedPQ ->
                    -1
    in
        case interval of
            Unison quality ->
                0 + perfectQualityNumber quality

            Second quality ->
                2 + imperfectQualityNumber quality

            Third quality ->
                4 + imperfectQualityNumber quality

            Fourth quality ->
                5 + perfectQualityNumber quality

            Fifth quality ->
                7 + perfectQualityNumber quality

            Sixth quality ->
                9 + imperfectQualityNumber quality

            Seventh quality ->
                11 + imperfectQualityNumber quality

            Octave quality ->
                12 + perfectQualityNumber quality


quality : Interval -> Quality
quality interval =
    case interval of
        Unison quality ->
            fromPerfectQuality quality

        Second quality ->
            fromImperfectQuality quality

        Third quality ->
            fromImperfectQuality quality

        Fourth quality ->
            fromPerfectQuality quality

        Fifth quality ->
            fromPerfectQuality quality

        Sixth quality ->
            fromImperfectQuality quality

        Seventh quality ->
            fromImperfectQuality quality

        Octave quality ->
            fromPerfectQuality quality


fromPerfectQuality : PerfectQuality -> Quality
fromPerfectQuality quality =
    case quality of
        PerfectPQ ->
            Perfect

        AugmentedPQ ->
            Augmented

        DiminishedPQ ->
            Diminished


fromImperfectQuality : ImperfectQuality -> Quality
fromImperfectQuality quality =
    case quality of
        MajorIQ ->
            Major

        MinorIQ ->
            Minor

        AugmentedIQ ->
            Augmented

        DiminishedIQ ->
            Diminished
