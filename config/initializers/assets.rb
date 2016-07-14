# Be sure to restart your server when you modify this file.

# Version of your assets, change this if you want to expire all your assets.
Rails.application.config.assets.version = '1.0'

# Add additional assets to the asset load path
# Rails.application.config.assets.paths << Emoji.images_path

# Precompile additional assets.
# application.js, application.css, and all non-JS/CSS in app/assets folder are already added.
# Rails.application.config.assets.precompile += %w( search.js )


# TODO: this defaults to true in sprockets-rails 3.0, but opal-rails still depends on <3.0
# See https://github.com/opal/opal-rails/issues/82
# Asset digests allow you to set far-future HTTP expiration dates on all assets,
# yet still be able to expire them through the digest params.
Rails.application.config.assets.digest = true

# TODO: this defaults to true in sprockets-rails 3.0, but opal-rails still depends on <3.0
# Adds additional error checking when serving assets at runtime.
# Checks for improperly declared sprockets dependencies.
# Raises helpful error messages.
Rails.application.config.assets.raise_runtime_errors = true
