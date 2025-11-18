
## makeCacheMatrix: این تابع یک آبجکت "ماتریس ویژه" ایجاد می‌کند 
## که می‌تواند معکوس خود (inverse) را کش (Cache) کند.

makeCacheMatrix <- function(x = matrix()) {
  # 'i' متغیری است که معکوس کش شده در آن ذخیره می‌شود (ابتدا NULL است).
  i <- NULL
  
  # 1. تابع برای تنظیم مجدد ماتریس (مقدار i را پاک می‌کند)
  set <- function(y) {
    x <<- y 
    i <<- NULL
  }
  
  # 2. تابع برای گرفتن ماتریس
  get <- function() x
  
  # 3. تابع برای تنظیم مقدار کش شده معکوس
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  # 4. تابع برای گرفتن مقدار کش شده معکوس
  getInverse <- function() i
  
  # لیستی از این توابع را برمی‌گرداند
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve: این تابع معکوس آبجکت makeCacheMatrix را محاسبه می‌کند.
## اگر معکوس قبلاً محاسبه شده باشد، آن را از کش برمی‌دارد.

cacheSolve <- function(x, ...) {
  # تلاش برای گرفتن معکوس از کش
  i <- x$getInverse()
  
  # اگر 'i' تهی (NULL) نباشد، یعنی از قبل محاسبه و کش شده است.
  if(!is.null(i)) {
    message("getting cached data")
    return(i) # معکوس کش شده را برمی‌گرداند
  }
  
  # اگر کش نشده بود (Cache Miss)، ماتریس را می‌گیرد.
  data <- x$get()
  
  # معکوس را با استفاده از تابع solve() محاسبه می‌کند
  i <- solve(data, ...)
  
  # معکوس محاسبه شده را کش می‌کند
  x$setInverse(i)
  
  # معکوس جدید را برمی‌گرداند
  return(i)
}