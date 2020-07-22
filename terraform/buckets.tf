resource "aws_s3_bucket" "b" {
  bucket = "getkimball-features-app"
  acl    = "private"

  tags = {
    Name        = "getkimball-features-app"
    Environment = "dev"
  }
}
