defmodule Pers do  # numele structurii
  defstruct nume: "Ana", ani: 23  # valori implicite
end

defmodule Catel do
  defstruct nume: "Rex", cuminte: true
end

defprotocol Mergator do
    def nr_picioare(data)
end

defimpl Mergator, for: Pers do
  def nr_picioare(pers), do: 2
end

defimpl Mergator, for: Catel do
  # nr_picioare neimplementat, vom primi warning
end

defimpl Mergator, for: Any do
  def nr_picioare(_), do: 0  # implicit, zero picioare
end

defmodule Sarpe do
  @derive[Mergator]  # fallback explicit, ii este suficienta implementarea cu zero
  defstruct [:nume, :lungime]
end


# pers = %Pers{}
# catel = %Catel{}
# sarpe = %Sarpe{}

# Mergator.nr_picioare(pers)
# Mergator.nr_picioare(catel)
# Mergator.nr_picioare(sarpe)
