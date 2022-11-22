package $package$.service

import zio._
import zio.mock.Expectation._
import zio.stream._
import zio.test._
import zio.test.Assertion._
import $package$.domain._
import $package$.service._
import $package$.service.ItemService._
import $package$.repo._

object ItemServiceSpec extends ZIOSpecDefault:

  val exampleItem = Item(ItemId(123), "foo")

  val getItemMock: ULayer[ItemRepository] = ItemRepoMock.GetById(
    equalTo(ItemId(123)),
    value(Some(exampleItem)),
  ) ++ ItemRepoMock.GetById(equalTo(ItemId(124)), value(None))

  val getByNonExistingId: ULayer[ItemRepository] =
    ItemRepoMock.GetById(equalTo(ItemId(124)), value(None))

  val updateMock: ULayer[ItemRepository] =
    ItemRepoMock.Update(
      hasField("id", _.id, equalTo(exampleItem.id)),
      value(Some(())),
    ) ++ ItemRepoMock.Update(
      hasField("id", _.id, equalTo(ItemId(124))),
      value(None),
    )

  def spec = suite("item service test")(
    test("get item id accept long") {
      for
        found  <- assertZIO(getItemById(ItemId(123)))(isSome(equalTo(exampleItem)))
        mising <- assertZIO(getItemById(ItemId(124)))(isNone)
      yield found && mising
    }.provide(getItemMock, ItemServiceLive.layer),
    suite("update item")(
      test("non existing item") {
        assertZIO(updateItem(ItemId(124), "bar").exit)(
          fails(equalTo(BusinessError("Item with ID 124 not found")))
        )
      }.provide(getByNonExistingId, ItemServiceLive.layer),
      test("update succesfull") {
        assertZIO(updateItem(ItemId(123), "bar"))(isUnit)
      }.provide(updateSuccesfullMock, ItemServiceLive.layer),
    ),
  )
