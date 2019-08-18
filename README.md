# Simulator

The simulator is an artificial micro-economy
consisting of nodes (people) who are related
to one amnother by virtue of tehir economic 
transactions.  Our initial aim with the simulator
is to gain insight into how complementary currency
interacts with the rules defining the economy.


## Operation

The simulator opetates as follows.  We
describe it in terms of the current test 
example. First, a set of 13 nodes is defined. 
Nodes are an abstraction of real people.  Each 
node has an internal state, as follows:

   Status: Recruited or NotRecuited
   Role: Unemployed or Shopkeeper
   Name: for now, names like "p1", "q5"
   ParentGraph: an integer (see below)
   Account: contains money belonging to the node
   
There are a few other state variables, but they are 
not important for this dicussion.  Shopkeers are 
rendered as squares, unemployed as circles. 
Their diameter increases as their account balance
increases.



Nodes belong to 
two networks (graphs). The first is a working network,
which initially has no edges.  That is, nodes are 
not related to eachother in any way.  The second
is the hidden network.  The hiden netwrok consists
of two disjoint subgraphs. Each is a "wheel": a hub-and-spoke
graph with one central node and all other ntodes in the subgraph
connected to it.  The central node is an "influencer." Opinions,
preferences, recommendations of the influencer carry great 
weight in the decisions of its respective influencees.

One node is designated the "recruiter.""  He attempts to recruit
nodes to his network as the game progresses.  Recruiting influencers is advantages.  When an influencer is recruited,
all of its influencees are recruited as well.

## Phases

The simulator, which may be viewed as a kind of game engine,
operates in two phases: recuitment, then trading.  During the recruitment phase, the player may click on nodes to recruit them to the network.  If he clicks on an influencer, the corresponding influencees are added.  Recruitees may also recruit nodes.  This happens at random.

During the second (trading) phase of the game, unemployed nodes can earn money taking care of the forest.  At the beginning of the game, there are 20 units of forest. Forest workers are compensated for their work, but also have to buy food and supplies from the shopkeepers.   In addtion, shopkeepers must pay fees on their shops (see rate chart).



## Game clock

Time in this artificial world is regulated by a Game Clock. It currently ticks at the rate of one per second; one tick represents one day. Transactions may occur daily, weekly, or biweekly.   x
- **Day zero:**

On each successive click of the Games clock the foll
