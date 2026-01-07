open import Level
open import Relation.Binary
open import Data.Product

infixr 20 _¦_
infix 10 _∈_
infix 10 _⊆_
infix 10 _≈_

variable
  ℓ : Level
  α : Set ℓ
  a b c : α

data S {ℓ : Level} : Set ℓ → Set (suc ℓ) where
  ∅   : ∀ {α} → S α
  _¦_ : ∀ {α} → α → S α → S α

variable
  x y z : S α

data _∈_ {ℓ : Level} {α : Set ℓ} : α → S α → Set (suc ℓ) where
  here     : a ∈ a ¦ x
  there    : a ∈ y → a ∈ b ¦ y

_⊆_ : {ℓ : Level} → {α : Set ℓ} → S α → S α → Set (suc ℓ)
x ⊆ x₁ = ∀ a → (a ∈ x) → (a ∈ x₁)

_≈_ : {ℓ : Level} → {α : Set ℓ} → S α → S α → Set (suc ℓ)
x ≈ x₁ = x ⊆ x₁ × x₁ ⊆ x

refl : Reflexive (_≈_ {ℓ = ℓ} {α = α})
refl = (λ a z → z) , (λ a z → z)

sym : Symmetric (_≈_ {ℓ = ℓ} {α = α})
sym = λ z → z .proj₂ , z .proj₁

trans : Transitive (_≈_ {ℓ = ℓ} {α = α})
trans = λ z z₁ →
           (λ a z₂ → z₁ .proj₁ a (z .proj₁ a z₂)) ,
           (λ a z₂ → z .proj₂ a (z₁ .proj₂ a z₂))
