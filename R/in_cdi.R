library(tidyverse)

cdi_words =
  c("airplane",	"alligator",	"animal",	"ankle",	"ant",	"apple",	"apple+sauce",	"applesauce",	"arm",	"aunt",
    "baby",	"babysitter",	"backyard",	"ball",	"balloon",	"banana",	"bananas",	"basement",	"basket",	"bat",	"bath",	"bathroom",	"bathtub",	"beach",	"beads",	"bean",	"beans",	"bear",	"bed",	"bedroom",	"bee",	"belly+button",	"belt",	"bench",	"bib",	"bicycle",	"bird",	"blanket",	"block",	"boat",	"book",	"boots",	"bottle",	"bottom",	"bowl",	"box",	"boy",	"bread",	"breakfast",	"broom",	"brother",	"brush",	"bubble",	"bubbles",	"bucket",	"bug",	"bunny",	"bus",	"butter",	"butterfly",	"button",
    "cake",	"camera",	"camping",	"can",	"candy",	"car",	"carrot ",	"carrots",	"cat",	"cereal",	"chair",	"chalk",	"cheek",	"cheerio ",	"cheerios",	"cheese",	"chicken",	"child",	"chin",	"chocolate",	"church",	"circus",	"clock",	"closet",	"cloud",	"clown",	"coat",	"coffee",	"coke",	"comb",	"cookie",	"cookies",	"corn",	"couch",	"country",	"cow",	"cowboy",	"cracker",	"crackers",	"crayon",	"crib",	"cup",
    "deer",	"diaper",	"dinner",	"dish",	"doctor",	"dog",	"doll",	"donkey",	"donut",	"door",	"downtown",	"drawer",	"dress",	"drink",	"dryer",	"duck",
    "ear",	"egg",	"eggs",	"elephant",	"eye",
    "face",	"farm",	"finger",	"fireman",	"firetruck",	"fish",	"flag",	"flower",	"food",	"foot",	"fork",	"french+fries",	"french+fry",	"frog",
    "game",	"garage",	"garbage",	"garden",	"gas+station",	"giraffe",	"girl",	"glass",	"glasses",	"glove",	"gloves",	"glue",	"goose",	"grape",	"grapes",	"grass",	"green+bean",	"green+beans",	"gum",
    "hair",	"hamburger",	"hammer",	"hand",	"hat",	"head",	"helicopter",	"hen",	"high+chair",	"home",	"horse",	"hose",	"house",
    "ice",	"ice+cream",
    "jacket",	"jar",	"jeans",	"jello",	"jelly",	"juice",
    "keys",	"kitchen",	"kitty",	"kleenex",	"knee",	"knife",
    "ladder",	"lady",	"lamb",	"lamp",	"lawn mower",	"leg",	"light",	"lion",	"lips",	"living room",	"lollipop",	"lunch",
    "mailman",	"man",	"meat",	"medicine",	"melon",	"milk",	"mittens",	"money",	"monkey",	"moon",	"moose",	"mop",	"motorcycle",	"mouse",	"mouth",	"movie",	"muffin",	"muffins",
    "nail",	"nap",	"napkin",	"necklace",	"noodles",	"nose",	"nurse",	"nuts",
    "orange",	"oranges",	"outside",	"oven",	"owl",
    "pajamas",	"pancake",	"pancakes",	"pants",	"paper",	"park",	"party",	"pattycake",	"peanut+butter",	"peas",	"peekaboo",	"pen",	"pencil",	"penguin",	"penis",	"penny",	"people",	"person",	"pet's name",	"pickle",	"pickles",	"picnic",	"picture",	"pig",	"pillow",	"pizza",	"plant",	"plate",	"play+dough",	"play+pen",	"playground",	"police",	"pony",	"pool",	"pop",	"popcorn",	"popsicle",	"porch",	"potato",	"potato+chip",	"potatoes",	"potty",	"present",	"pretzel",	"pretzels",	"pudding",	"pumpkin",	"puppy",	"purse",	"purse",	"puzzle",
    "radio",	"rain",	"raisin",	"raisins",	"refrigerator",	"rock",	"rocking+chair",	"roof",	"room",	"rooster",
    "salt",	"sandbox",	"sandwich",	"sauce",	"scarf",	"school",	"scissors",	"sheep",	"shirt",	"shoe",	"shopping",	"shorts",	"shoulder",	"shovel",	"shower",	"sidewalk",	"sink",	"sister",	"sky",	"sled",	"slide",	"slipper",	"snack",	"sneaker",	"snow",	"snowman",	"snowsuit",	"soap",	"sock",	"soda",	"sofa",	"soup",	"spaghetti",	"spoon",	"sprinkler",	"squirrel",	"stairs",	"star",	"stick",	"stone",	"store",	"story",	"stove",	"strawberries",	"strawberry",	"street",	"stroller",	"sun",	"sweater",	"swing",
    "table",	"tape",	"teacher",	"teddy+bear",	"teddybear",	"telephone",	"temple",	"tiger",	"tights",	"tissue",	"toast",	"toe",	"tongue",	"tooth",	"toothbrush",	"towel",	"toy",	"tractor",	"train",	"trash",	"tray",	"tree",	"tricycle",	"truck",	"tummy",	"tuna",	"turkey",	"turtle",	"TV",
    "uncle",	"underpants",
    "vacuum",	"vagina",	"vanilla",	"vitamin",	"vitamins",
    "walker",	"washing+machine",	"watch",	"water",	"wind",	"window",	"wolf",	"woods",	"work",
    "yard",	"yogurt",
    "zebra",	"zipper",	"zoo")

on_cdi <- function(input) {
  mutate(input, in_cdi = basic_level %in% cdi_words)
}
