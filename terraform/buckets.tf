resource "aws_s3_bucket" "app_bucket" {
  bucket = "getkimball-features-app"
  acl    = "private"

  tags = {
    Name        = "getkimball-features-app"
    Environment = "dev"
  }
}

resource "aws_s3_bucket" "api_app_bucket" {
  bucket = "getkimball-api-app"
  acl    = "private"

  tags = {
    Name        = "getkimball-api-app"
    Environment = "dev"
  }
}

resource "github_actions_secret" "s3_bucket" {
  repository      = "features"
  secret_name     = "S3_BUCKET"
  plaintext_value = aws_s3_bucket.api_app_bucket.id
}
