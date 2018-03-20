<!---
uncomment the following section, by removing the <!> (but keep ---) to create pdf using pandoc.
--->
<!---
title: Documentation for the \textit{hub model}
author: Nicholas Molyneaux
header-includes: |
    \usepackage[left=2cm,right=2cm,top=3cm,bottom=2.5cm,headheight=1cm]{geometry}
    \usepackage{fancyhdr}
    \pagestyle{fancy}
    \fancyhead[CO,CE]{Hub model doc}
    \fancyfoot[CO,CE]{TRANS-FORM}
    \fancyfoot[LE,RO]{\thepage}
--->

# Introduction #
Welcome to the hub-model simulator ! This project is conducted within the scope of the TRANS-FORM project and a PhD
thesis. The specification of the input formats for running the simulation and information on how to run the simulation
can be found below. At this stage, a very brief summray of the key steps is provided:

1. install scala and sbt
2. download and publish locally the tools repository from https://github.com/NicholasMolyneaux/tools
3. download and compile this project
4. write the configuration file
5. create the input files (infrastructure and demand)
6. run the simulation

# Input data #
The input data is composed of two categories of files: the infrastructure specification and the deman specification.
Detailed descriptions and examples of all of these files can be found in the following subsections.

## Infrastructure specification ##
The infrastructure must be specified in two files, the first contains the collection of walls and the second
contains the specification of the graph used for route choice with the management strategy specifications.

### Wall specification file ###
A wall is described as a line. Each wall also contains a comment field, which is only used for debugging and for
plotting. The last property of each wall is the "type" field, indicating whether the wall is part of the outer
shell or not. This leads to the following:

* comment: humand-readible information, used for debugging
* x1: x coordinate of start of wall
* y1: y coordinate of start of wall
* x2: x coordinate of end of wall
* y2: y coordinate of end of wall
* type: indicator whether the wall belongs to the outer shell or not (0 for true, 1 for false)

**Example**
Two other elements are passed in the JSON file: the ''location'' and and ''sublocation'' fields. These are used
mainly for readibility reasons. Below is a full example:
```json
{
  "location": "lausanne",
  "sublocation": "PIW",
  "walls": [
    {
      "comment": "pl56-W",
      "x1": 36.18,
      "y1": 10.72,
      "x2": 36.18,
      "y2": 2.68,
      "type": 0
    }, {
      "comment": "pl56-W",
      "x1": 36.18,
      "y1": 2.68,
      "x2": 39.865,
      "y2": 2.68,
      "type": 0
    }, {
      "comment": "pl56-W",
      "x1": 39.865,
      "y1": 2.68,
      "x2": 39.865,
      "y2": 10.72,
      "type": 0
    }
  ]
}
```

### Graph specification file ###
The graph is composed of two collections. The first is the collection of zones, and the second is the collection
of connections between the zones. Alongside the graph specification, the possible management strategies are also
defined. These can be empty if no management strategy is passed. The zones are defined as follows:
 
* name: unique name of the zone
* x: x-coord of center of the zone (obsolete)
* y: y-coord of center of the zone (obsolete)
* x1: x-coord of bottom left
* y1: y-coord of bottom left
* x2: x-coord of bottom right
* y2: y-coord of bottom right
* x3: x-coord of top right
* y3: y-coord of top right
* x4: x-coord of top left
* y4: y-coord of top left

The order of these corners __MUST__ be respected.

The connectivity specification is:
 
 * node: name of the current node
 * connectivity: connections from the current node to the nodes listed here (directed)
 
The connections are directed, hence there must be as many connection objects as nodes. In this context, the terms
"node" and "zone" can be used in an interchangable manner.

**Example**
As for the walls file the ''location'' and and ''sublocation'' fields must exist. They are not used by the
simulation but must still be present.
```json
{
  "location": "lausanne",
  "sublocation": "test",
  "nodes": [
    {
      "name": "a",
      "x": 0.0,
      "y": 0.0,
      "x1": 47.71234866828081,
      "y1": 247.8312348668281,
      "x2": 47.71234866828078,
      "y2": 188.6055690072639,
      "x3": 113.466828087167,
      "y3": 188.6055690072639,
      "x4": 113.466828087167,
      "y4": 247.8312348668281
    }, {
      "name": "b",
      "x": 0.0,
      "y": 0.0,
      "x1": 334.5138014527845,
      "y1": 169.9518159806295,
      "x2": 415.1912832929783,
      "y2": 169.9518159806295,
      "x3": 415.1912832929783,
      "y3": 238.9707021791768,
      "x4": 334.5138014527845,
      "y4": 238.9707021791768
    }, {
      "name": "c",
      "x": 0.0,
      "y": 0.0,
      "x1": 239.2021395489995,
      "y1": 154.7129236207632,
      "x2": 273.3027061212375,
      "y2": 121.4648712128312,
      "x3": 317.6334426651468,
      "y3": 132.5475553488085,
      "x4": 294.6155602288862,
      "y4": 168.3531502496584
    }
  ],
  "connectivity": [
    {
      "node": "c",
      "connected_to": ["b"]
    }, {
      "node": "a",
      "connected_to": ["b", "c"]
    }, {
      "node": "b",
      "connected_to": ["a"]
    }
  ],
  "flow_gates": [],
  "controlled_areas": [],
  "binary_gates": [],
  "flow_separators": []
}
```

## Definition of the demand ##
As the objective is to simulate transportation hubs (excluding airports) the pedestrian demand can come from two
distinct origins: public transport vehicles and walking pedestrians. This data is passed as JSON files to the
simulator.

### Timetable specification ###
The arrival time, departure time, platform and train specifictions are provided in the timetable file. The fields
which must be completed are the following:
 
 * id: unique identifier of the vehicle
 * type: type of the vehicle
 * track: track/platform/shelter where the vehicle arrives
 * arrival-time: arrival time of the vehicle (time of day)
 * departure-time: departure time of the vehicle (time of day)
 * capacity: maximum capacity of the vehicle

Although the terminolgy refers to train, any type of public transport vehicle can be used. Buses, trams and trains
can be freely combined. The "type" field can be used to identify classes of vehicles. A second element must be
included in the timetable specification file: the track to zone mapping. This "map" links the platform to a
set of zones where the passengers will disembark/embark.

**Example**
```json
{
  "location": "lausanne",
  "trains": [
    {
      "id": "12217",
      "type": "S21",
      "track": 3,
      "arrival-time": "07:05:00",
      "departure-time": "07:07:00",
      "capacity": 515
    }, {
      "id": "12218",
      "type": "S2",
      "track": 4,
      "arrival-time": "07:06:00",
      "departure-time": "07:08:00",
      "capacity": 517
    }
  ],
  "track2nodes": [
    {
      "track": 4,
      "nodes": ["11", "12"]
    }, {
      "track": 3,
      "nodes": ["9", "10"]
    }
  ]
}
```

### Pedestrian flow specification ###
The pedestrian flows between the different public tranports vehicles and places in the transportation hub
can be specified int wo ways, which can be freely combined. The first is a flow-based specification and the second
is a disaggregate approach.

#### Flow-based specification ####
There are two types of flows: flows originating from public transport vehicles and flows originating from a "fixed
location". When passengers disembark from a vehicle, they immediately move towards their destination. Hence the
time at which they enter the system depends on the arrival time of the vehicle inside the hub. The fields whch
define these flows are the following:
 
 * origin: id of the originating vehicle
 * destination: id of the destination node or vehicle
 * flow: number of pedestrians walking this trip

Pedestrians arriving from a "fixed" location do not depend on some sort of scheduled transportation system, hence
they arrive independently from one another. They tend to follow a Poisosn process. The requeried fields are:
 
 * origin: origin node of the flow
 * destination: destination node or vehicle of the flow
 * start: start time of the flow
 * end: end time of the flow
 * flow: number of pedestrians to be generated in the interval

**Example**
Below a sample file is available:
```json
{
  "location": "lausanne",
  "PTflows": [
    {
      "origin": "T_12217",
      "destination": "T_12218",
      "flow": 80
    }, {
      "origin": "T_12217",
      "destination": "S_13",
      "flow": 120
    }, {
      "origin": "T_12217",
      "destination": "S_14",
      "flow": 100
    }
  ],
  "flows": [
    {
      "origin": "1",
      "destination": "14",
      "start": "07:00:00",
      "end": "08:00:00",
      "flow": 180
    }, {
      "origin": "1",
      "destination": "13",
      "start": "07:00:00",
      "end": "08:00:00",
      "flow": 180
    }
  ]
}
```

#### Disaggregate demand ####
The disaggregate pedestrian input can also be used. This has been specifically developped for transfering data
between the urban model and hub model in the context of the TRANS-FORM project. This data is basically a disaggregate
OD matrix. Each entry corresponds to a pedestrian, and the fields to be filled are the following:
 
 * ID: unique ID of the pedestrian
 * O: origin zone of the pedestrian
 * D: destintation zone of the pedestrian
 * entryTime: time at which the pedestrian will enter the simulation environment

**Example**
The data is stored using JSON. Below is a sample. In this sample the exitTime is provided, but this is not required
for running the simulations.
```json
[
  { "ID":"1", "O":"1", "D":"6", "entryTime": 25200.336, "exitTime":25223.771},
  { "ID":"3", "O":"7", "D":"6", "entryTime": 25201.366, "exitTime":25271.026},
  { "ID":"5", "O":"14", "D":"8", "entryTime": 25201.559, "exitTime":25241.09},
  { "ID":"6", "O":"13", "D":"8", "entryTime": 25201.595, "exitTime":25270.086},
  { "ID":"7", "O":"12", "D":"8", "entryTime": 25201.863, "exitTime":25249.366},
  { "ID":"9", "O":"12", "D":"8", "entryTime": 25205.135, "exitTime":25255.47},
  { "ID":"10", "O":"12", "D":"6", "entryTime": 25205.565, "exitTime":25275.628},
  { "ID":"11", "O":"12", "D":"7", "entryTime": 25207.035, "exitTime":25246.846}
]
```
# Running the simulation #
The hub simulator is coded in Scala (https://www.scala-lang.org/) and the compilation/execution is managed by
sbt (https://www.scala-sbt.org/). This combination makes the sharing of the code convient and the integration of packages
simple thanks to the large database available at Maven (https://mvnrepository.com/). Therefore to be able to compile the
hub-model one needs to install **scala** and **sbt**. 
## Dependencies ##
The code depends on multiple mutliples, most of which can be automatically downloaded from maven thanks to sbt. There
are a few libraries which have been developped in-house, meaning they cannot be downloaded automatically from maven. 
These libraries are available on github (https://github.com/NicholasMolyneaux/tools). They can be compiled and packaged
locally thanks to sbt as well.
```sbtshell
libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.6.5",
  "org.scalanlp" %% "breeze" % "0.13",
  "org.scalanlp" %% "breeze-natives" % "0.13",
  "org.jgrapht" % "jgrapht-core" % "1.0.1",
  "com.github.scopt" %% "scopt" % "3.6.0",
  "org.jcodec" % "jcodec-javase" % "0.2.0",
  "com.typesafe" % "config" % "1.3.1",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "transpor.tools" % "power-voronoi" % "1.0",
  "transpor.molyneaux" %% "scala-custom" % "1.0-SNAPSHOT",
  "transpor.tools" % "dxf-parser" % "1.0"
)

resolvers ++= Seq(
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)
```

## Configuration file ##
In order to configure the simulations, a config file is used. This means the code doesn't have to be re-compiled for
different scenarios. The library used for parsing the config file can be found here (https://github.com/lightbend/config).
It is automatically included thanks to the build file used by sbt. The syntax is that of "HOCON" 
(https://github.com/lightbend/config/blob/master/HOCON.md), a cleaned version of JSON. This configuration file must be
 passed as a command line argument to the program. There are five main categories, each of which have multiple parameters: 

* files: location of input files
* sim: simulation parameters
* output: selection of outputs to create
* execution: parameters regarding the parallel execution
* results-analysis: processing of the results

The configuration file must be located in the following folder so that sbt can find it.
```
src/main/resources/
```
## Execution ##
The suggest way to run the simulations is to use sbt. This way, only the configuration needs to be passed to the program.
Once running sbt, one needs to run the following command:
```sbtshell
run --conf configuration-file.conf
```
Naturally, the name of the configuration file can be changed. The outputs are placed in the same folder as the build.sbt
file. So make sure that mulitple simulations have a different prefix (one of the parameters in the config file) to avoid
deleting previous results.


 