# elm-music-theory
Music theory library for working with notes, intervals, chords, scales, and keys.

## Usage

### Note
There are three flavors of notes, each appropriate for different situations:

#### PitchClass
A note name, like "C#", with no octave or duration specified. Very useful for basic analysis.

```
pitchClass (C Sharp)
    |> PitchClass.toString

➜ "C♯"
```

#### The Note type
There are also `Note`s with an octave given, and with both an octave and a duration given. These aren't of much use now, but will be once this library is able to generate chord voicings, melody lines, etc.

### Intervals
A distance between two pitch classes.
```
Interval.semitoneDistance <| interval MajorSeventh
    
➜ 11

SpellIntervals.getPitchClassAtIntervalFrom (pitchClass A Natural) (interval MajorSeventh)
    |> PitchClass.toString

➜ "G♯"
```

### Chord
A group of pitch classes to be played at once, with specific interval relationships.

```
Chord.seventh F Sharp MajorSeven
    |> Chord.toString

➜ "F♯Δ"

Chord.seventh F Sharp MajorSeven
    |> Chord.pitchClasses
    |> List.map PitchClass.toString

➜ [ "F♯", "A♯", "C♯", "E♯" ]
```

### Scale
A group of pitch classes from which chords or melodies can be created.

```
Scale.dorian (pitchClass C Natural)
    |> pitchClassesInScale
    |> List.map PitchClass.toString
    
➜ [ "D", "E", "F", "G", "A", "B", "C" ]
```

### Key
A major or minor scale that defines a tonality.

```
Key.major (pitchClass B Flat)
    |> Key.accidentals
    |> List.map PitchClass.toString
    
➜ [ "B♭", "E♭" ]
```

### ChordsInKeys
The `ChordsInKeys` module can analyze chord/key relationships:

The chords at degrees of the scale:
```
Key.major (pitchClass D Flat)
    |> List.map (ChordsInKeys.seventhChordAtDegree) [ I, II, III, IV, V, VI, VII ]
    |> List.map Chord.toString

➜ [ "A♭7", "B♭-7", "C-7♭5", "D♭Δ", "E♭-7", "F-7", "G♭Δ" ]
```
The keys a chord belongs to:
```
Chord.triad C Natural Major
    |> ChordsInKeys.keysForChord
    |> List.map Key.toString
    
➜ [ "C major", "A minor", "F major", "D minor", "G major", "E minor" ]
```
