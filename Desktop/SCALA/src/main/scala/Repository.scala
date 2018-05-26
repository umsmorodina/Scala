trait Repository {
  def store(poll: Poll): Unit
  def get(id: Int):Option[Poll]
  def all():Seq[Poll]
  def delete(id:Int):Unit
  def isLaunch(id:Int):Boolean
  def isContains(id:Int):Boolean
  def hasStart(id:Int):Boolean
  def start(id:Int):Unit
  def hasEnd(id:Int):Boolean
  def end(id:Int):Unit
  def getIndex():Int
  def isAdmin(id:Int, userId : Int):Boolean
}
