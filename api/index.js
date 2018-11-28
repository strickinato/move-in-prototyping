const R = require('ramda');
const v2 = "appcPwpCqAgBuCaBT";
const Airtable = require("airtable");
const base = new Airtable({apiKey: process.env.AIRTABLE_API_KEY}).base(v2);

const ITEM_ROOM_FIELD = "Can Be Used In";
const ITEM_CATEGORY_FIELD = "Type";

const getRooms = async () => {
  return base('Rooms').select({view: "Grid view"}).firstPage();
}

const getCategories = async () => {
  return base('Categories').select({view: "Grid view"}).firstPage();
}

const getItemCards = async () => {
  return base('Items').select({view: "Grid view"}).firstPage();
}

const getActionCards = async () => {
  return base('Action Cards').select({view: "Grid view"}).firstPage();
}

module.exports = async (req, res) => {
  const _rooms = await getRooms();
  const _categories = await getCategories();
  const _itemCards = await getItemCards();
  const _actionCards = await getActionCards();

  // for Development CORS
  // res.setHeader('Access-Control-Allow-Origin', "http://localhost:8000");

  const roomsById = R.zipObj(
    _rooms.map( (room) => { return room.id; } ),
    _rooms.map( (room) => { return room.fields["Name"]; } )
  );

  const categoriesById = R.zipObj(
    _categories.map( (category) => { return category.id; } ),
    _categories.map( (category) => { return category.fields["Name"]; } )
  );

  const itemCards = _itemCards
        .map( (card) => { return card.fields; } )
        .map( (card) => {
          let roomIds = card[ITEM_ROOM_FIELD];
          let roomNames;
          if (roomIds !== undefined) {
            roomNames = roomIds.map( (id) => { return roomsById[id]; } );
          } else {
            roomNames = ["Any"];
          }

          return Object.assign( {"rooms": roomNames}, card);
        })
        .map( (card) => {
          let categoryIds = card[ITEM_CATEGORY_FIELD];
          let categoryNames;
          if (categoryIds !== undefined) {
            categoryNames = categoryIds.map( (id) => { return categoriesById[id]; } );
          } else {
            categoryNames = ["None"];
          }

          return Object.assign( {"categories": categoryNames}, card);
        })
        .map( (card) => { return Object.assign( {"_type": "item"}, card); });

  const actionCards = _actionCards
        .map( (card) => { return card.fields; })
        .map( (card) => { return Object.assign( {"_type": "action"}, card); });

  const cards = R.concat(itemCards, actionCards);

  res.end(JSON.stringify({ cards }));
}
